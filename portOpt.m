% Import rets first as a matrix of all numeric columns without NA
clear
% d('/Users/Chris/Downloads/')
% rets = readmatrix('gbrets.csv', 'HeaderLines', 1)
% rets = readmatrix('gb100.csv', 'HeaderLines', 1)
cd('/Users/Chris/Documents/OneDrive - Imperial College London/Thesis/Code/')
% rets = readmatrix('us100.csv', 'HeaderLines', 1)
rets = readmatrix('us1000.csv', 'HeaderLines', 1)
rets(:, 1) = []
rets(isnan(rets)) = 0

% columns of returns data
a = size(rets,2)
x = zeros(a,1)
x(1) = 1
k = @(x) kurtosis(rets*x)

% 1000 and 3000 default options
% UseParallel not possible for this optimisation
options = optimset('MaxIter', 5e4, 'MaxFunEvals', 1e5, 'PlotFcns', @optimplotfval)%, 'UseParallel', true)
negSk = @(x) -skewness(rets*x)
maxSk = fmincon(negSk, ones(a, 1)/a, -eye(a), zeros(a,1), ones(1,a), 1, [],[],[], options)
optSkew = skewness(maxSk)
skewPort = rets*maxSk

minK = fmincon(k, ones(a, 1)/a, -eye(a), zeros(a,1), ones(1,a), 1, [],[],[], options)
optKurt = k(minK)
kurtPort = rets*minK

[optSkew, optKurt]

mk = @(x) 1e12 * var(rets*x)^2 / kurtosis(rets*x)
minMK = fmincon(mk, ones(a, 1)/a, -eye(a), zeros(a,1), ones(1,a), 1, [],[],[], options)
mkPort = rets*minMK

% MV opts
negMean = @(x) -100 * mean(rets*x)
maxMean = fmincon(negMean, ones(a, 1)/a, -eye(a), zeros(a,1), ones(1,a), 1, [],[],[], options)
v = @(x) 100 * var(rets*x)
minVar = fmincon(v, ones(a, 1)/a, -eye(a), zeros(a,1), ones(1,a), 1, [],[],[], options)
meanPort = rets*maxMean
varPort = rets*minVar

mos = @(x) [mean(rets*x), var(rets*x), skewness(rets*x), kurtosis(rets*x)]
mvsk = [mos(maxMean); mos(minVar); mos(maxSk); mos(minK)]

meanMax = mean(rets*maxMean)
varMin = var(rets*minVar)
skewMax = skewness(rets*maxSk)
kurtMin = kurtosis(rets*minK)
l = [1,1,1,1]
% Takes weights and lambda
mp = @(x, l) abs(1-mean(rets*x)/meanMax)^l(1) + abs(var(rets*x)/varMin-1)^l(2) + abs(1-skewness(rets*x)/skewMax)^l(3) + abs(kurtosis(rets*x)/kurtMin-1)^l(4)
% Takes returns and lambda
mpRet = @(r, l) abs(1-mean(r)/meanMax)^l(1) + abs(var(r)/varMin-1)^l(2) + abs(1-skewness(r)/skewMax)^l(3) + abs(kurtosis(r)/kurtMin-1)^l(4)

mp1100 = fmincon(@(x) mp(x,[1,1,0,0]), ones(a, 1)/a, -eye(a), zeros(a,1), ones(1,a), 1, [],[],[], options)
mp1010 = fmincon(@(x) mp(x,[1,0,1,0]), ones(a, 1)/a, -eye(a), zeros(a,1), ones(1,a), 1, [],[],[], options)
mp1001 = fmincon(@(x) mp(x,[1,0,0,1]), ones(a, 1)/a, -eye(a), zeros(a,1), ones(1,a), 1, [],[],[], options)
mp1110 = fmincon(@(x) mp(x,[1,1,1,0]), ones(a, 1)/a, -eye(a), zeros(a,1), ones(1,a), 1, [],[],[], options)
mp1111 = fmincon(@(x) mp(x,[1,1,1,1]), ones(a, 1)/a, -eye(a), zeros(a,1), ones(1,a), 1, [],[],[], options)
mp2111 = fmincon(@(x) mp(x,[2,1,1,1]), ones(a, 1)/a, -eye(a), zeros(a,1), ones(1,a), 1, [],[],[], options)
mp1211 = fmincon(@(x) mp(x,[1,2,1,1]), ones(a, 1)/a, -eye(a), zeros(a,1), ones(1,a), 1, [],[],[], options)
mp1121 = fmincon(@(x) mp(x,[1,1,2,1]), ones(a, 1)/a, -eye(a), zeros(a,1), ones(1,a), 1, [],[],[], options)
mp1112 = fmincon(@(x) mp(x,[1,1,1,2]), ones(a, 1)/a, -eye(a), zeros(a,1), ones(1,a), 1, [],[],[], options)
mp1122 = fmincon(@(x) mp(x,[1,1,2,2]), ones(a, 1)/a, -eye(a), zeros(a,1), ones(1,a), 1, [],[],[], options)

mvskMP = [mos(mp1111); mos(mp2111); mos(mp1121); mos(mp1112)]

mos(mp1100)
mos(mp1010)
mos(mp1001)
mos(mp1110)
mos(mp1111)
mos(mp2111)
mos(mp1211)
mos(mp1121)
mos(mp1112)
mos(mp1122)

mpDist = fitdist(rets*mp1111, 'Normal')
figure(); ksdensity(rets*mp1111); hold on; line(-.03:.001:.03, normpdf(-.03:.001:.03, mpDist.mu, mpDist.sigma), 'Color', 'r')

mpRet(rets*mp1111, [1,1,1,1])

clear
cd('/Users/Chris/Documents/OneDrive - Imperial College London/Thesis/Code/')
facs = readmatrix('factors.csv', 'HeaderLines', 1)
facs(:, [1,2,8,13]) = []
facs(isnan(facs)) = 0
facs = facs / 100

options = optimset('MaxIter', 5e4, 'MaxFunEvals', 1e5, 'PlotFcns', @optimplotfval)%, 'UseParallel', true)
b = size(facs, 2)
facMean = fmincon(@(x) -100 * mean(facs*x), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
facVar = fmincon(@(x) var(facs*x), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
facSkew = fmincon(@(x) -skewness(facs*x), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
facKurt = fmincon(@(x) kurtosis(facs*x), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
meanMax = mean(facs*facMean)
varMin = var(facs*facVar)
skewMax = skewness(facs*facSkew)
kurtMin = kurtosis(facs*facKurt)

facMP = @(x, l) abs(1-mean(facs*x)/meanMax)^l(1) + abs(var(facs*x)/varMin-1)^l(2) + abs(1-skewness(facs*x)/skewMax)^l(3) + abs(kurtosis(facs*x)/kurtMin-1)^l(4)

facDist = fitdist(facs*facKurt, 'Normal')
figure(); ksdensity(facs*facKurt); hold on; line(-.03:.001:.03, normpdf(-.03:.001:.03, facDist.mu, facDist.sigma), 'Color', 'r')

% Optimising for all factors
facVar = fmincon(@(x) facMP(x,[0,1,0,0]), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
facSkew = fmincon(@(x) facMP(x,[0,0,1,0]), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
facKurt = fmincon(@(x) facMP(x,[0,0,0,1]), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
fac1100 = fmincon(@(x) facMP(x,[1,1,0,0]), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
fac1010 = fmincon(@(x) facMP(x,[1,0,1,0]), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
fac1001 = fmincon(@(x) facMP(x,[1,0,0,1]), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
fac1110 = fmincon(@(x) facMP(x,[1,1,1,0]), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
fac1111 = fmincon(@(x) facMP(x,[1,1,1,1]), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
fac2111 = fmincon(@(x) facMP(x,[2,1,1,1]), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
fac1211 = fmincon(@(x) facMP(x,[1,2,1,1]), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
fac1121 = fmincon(@(x) facMP(x,[1,1,2,1]), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
fac1112 = fmincon(@(x) facMP(x,[1,1,1,2]), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
fac1122 = fmincon(@(x) facMP(x,[1,1,2,2]), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)

% Value, Quality and Momentum facs(:,[2,5,7])
facMP(facVar, [0,1,0,0])
facMP(facSkew, [0,0,1,0])
facMP(facKurt, [0,0,0,1])
facMP(fac1100, [1,1,0,0])
facMP(fac1010, [1,0,1,0])
facMP(fac1001, [1,0,0,1])
facMP(fac1110, [1,1,1,0])
facMP(fac1111, [1,1,1,1])
facMP(fac2111, [2,1,1,1])
facMP(fac1211, [1,2,1,1])
facMP(fac1121, [1,1,2,1])
facMP(fac1112, [1,1,1,2])
facMP(fac1122, [1,1,2,2])

% Value, Quality and Momentum facs(:,[2,5,7])
valmom=fac1111*0
valmom([2,5,7]) = 1/size(valmom([2,5,7]),1)
% Scores for equal-weighted portfolio
facMP(valmom, [0,1,0,0])
facMP(valmom, [0,0,1,0])
facMP(valmom, [0,0,0,1])
facMP(valmom, [1,1,0,0])
facMP(valmom, [1,0,1,0])
facMP(valmom, [1,0,0,1])
facMP(valmom, [1,1,1,0])
facMP(valmom, [1,1,1,1])
facMP(valmom, [2,1,1,1])
facMP(valmom, [1,2,1,1])
facMP(valmom, [1,1,2,1])
facMP(valmom, [1,1,1,2])
facMP(valmom, [1,1,2,2])


% Returns of value, quality, momentum
facsVM = facs(:,[2,5,7])
b = size(facsVM, 2)
% Optimise on just the 3 factors
facMPVM = @(x, l) abs(1-mean(facsVM*x)/meanMax)^l(1) + abs(var(facsVM*x)/varMin-1)^l(2) + abs(1-skewness(facsVM*x)/skewMax)^l(3) + abs(kurtosis(facsVM*x)/kurtMin-1)^l(4)
facVMVar = fmincon(@(x) facMPVM(x,[0,1,0,0]), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
facVMSkew = fmincon(@(x) facMPVM(x,[0,0,1,0]), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
facVMKurt = fmincon(@(x) facMPVM(x,[1,0,0,1]), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
facVM1100 = fmincon(@(x) facMPVM(x,[1,1,0,0]), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
facVM1010 = fmincon(@(x) facMPVM(x,[1,0,1,0]), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
facVM1001 = fmincon(@(x) facMPVM(x,[1,0,0,1]), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
facVM1110 = fmincon(@(x) facMPVM(x,[1,1,1,0]), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
facVM1111 = fmincon(@(x) facMPVM(x,[1,1,1,1]), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
facVM2111 = fmincon(@(x) facMPVM(x,[2,1,1,1]), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
facVM1211 = fmincon(@(x) facMPVM(x,[1,2,1,1]), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
facVM1121 = fmincon(@(x) facMPVM(x,[1,1,2,1]), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
facVM1112 = fmincon(@(x) facMPVM(x,[1,1,1,2]), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
facVM1122 = fmincon(@(x) facMPVM(x,[1,1,2,2]), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)

% Scores for optimised 3 factor portfolios 
facMPVM(facVMVar, [0,1,0,0])
facMPVM(facVMSkew, [0,0,1,0])
facMPVM(facVMKurt, [0,0,0,1])
facMPVM(facVM1100, [1,1,0,0])
facMPVM(facVM1010, [1,0,1,0])
facMPVM(facVM1001, [1,0,0,1])
facMPVM(facVM1110, [1,1,1,0])
facMPVM(facVM1111, [1,1,1,1])
facMPVM(facVM2111, [2,1,1,1])
facMPVM(facVM1211, [1,2,1,1])
facMPVM(facVM1121, [1,1,2,1])
facMPVM(facVM1112, [1,1,1,2])
facMPVM(facVM1122, [1,1,2,2])
