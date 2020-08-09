clear
cd('/Users/Chris/Documents/OneDrive - Imperial College London/Thesis/Code/')
etf = readmatrix('etfData.csv', 'HeaderLines', 1)
etf(:,1) = []
etf(isnan(etf)) = 0
figure(); plot(cumsum(etf))

options = optimset('MaxIter', 5e4, 'MaxFunEvals', 1e5, 'PlotFcns', @optimplotfval)%, 'UseParallel', true)
b = size(etf, 2)
etfMean = fmincon(@(x) -100 * mean(etf*x), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
etfVar = fmincon(@(x) var(etf*x), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
etfSkew = fmincon(@(x) -skewness(etf*x), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
etfKurt = fmincon(@(x) kurtosis(etf*x), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
meanMax = mean(etf*etfMean)
varMin = var(etf*etfVar)
skewMax = skewness(etf*etfSkew)
kurtMin = kurtosis(etf*etfKurt)

etfMP = @(x, l) abs(1-mean(etf*x)/meanMax)^l(1) + abs(var(etf*x)/varMin-1)^l(2) + abs(1-skewness(etf*x)/skewMax)^l(3) + abs(kurtosis(etf*x)/kurtMin-1)^l(4)

etfDist = fitdist(etf*etfKurt, 'Normal')
figure(); ksdensity(etf*etfKurt); hold on; line(-.03:.001:.03, normpdf(-.03:.001:.03, etfDist.mu, etfDist.sigma), 'Color', 'r')

% Optimising for all etftors
etf1100 = fmincon(@(x) etfMP(x,[1,1,0,0]), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
etf1010 = fmincon(@(x) etfMP(x,[1,0,1,0]), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
etf1001 = fmincon(@(x) etfMP(x,[1,0,0,1]), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
etf1110 = fmincon(@(x) etfMP(x,[1,1,1,0]), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
etf1111 = fmincon(@(x) etfMP(x,[1,1,1,1]), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
etf2111 = fmincon(@(x) etfMP(x,[2,1,1,1]), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
etf1211 = fmincon(@(x) etfMP(x,[1,2,1,1]), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
etf1121 = fmincon(@(x) etfMP(x,[1,1,2,1]), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
etf1112 = fmincon(@(x) etfMP(x,[1,1,1,2]), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
etf1122 = fmincon(@(x) etfMP(x,[1,1,2,2]), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)

figure(); plot(cumsum(etf*etf1122))
