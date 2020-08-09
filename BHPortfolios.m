clear
cd('/Users/Chris/Documents/OneDrive - Imperial College London/Thesis/Code/')
bh = readmatrix('BH2000.csv', 'HeaderLines', 1)
bh(:,1) = []
bh(isnan(bh)) = 0
figure(); plot(cumsum(bh))
bhW = readmatrix('bhWeights.csv', 'HeaderLines', 1)
bhRets = bh * bhW
bhWB4 = bhW
bhWB4(26) = 0
bhWB4 = bhWB4 / sum(bhWB4)

options = optimset('MaxIter', 5e4, 'MaxFunEvals', 1e5, 'PlotFcns', @optimplotfval)%, 'UseParallel', true)
b = size(bh, 2)
bhMean = fmincon(@(x) -100 * mean(bh*x), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
bhVar = fmincon(@(x) var(bh*x), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
bhSkew = fmincon(@(x) -skewness(bh*x), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
bhKurt = fmincon(@(x) kurtosis(bh*x), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
meanMax = mean(bh*bhMean)
varMin = var(bh*bhVar)
skewMax = skewness(bh*bhSkew)
kurtMin = kurtosis(bh*bhKurt)

bhMP = @(x, l) abs(1-mean(bh*x)/meanMax)^l(1) + abs(var(bh*x)/varMin-1)^l(2) + abs(1-skewness(bh*x)/skewMax)^l(3) + abs(kurtosis(bh*x)/kurtMin-1)^l(4)

bhDist = fitdist(bh*bhKurt, 'Normal')
figure(); ksdensity(bh*bhKurt); hold on; line(-.03:.001:.03, normpdf(-.03:.001:.03, bhDist.mu, bhDist.sigma), 'Color', 'r')

% Optimising for all bhtors
bh1100 = fmincon(@(x) bhMP(x,[1,1,0,0]), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
bh1010 = fmincon(@(x) bhMP(x,[1,0,1,0]), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
bh1001 = fmincon(@(x) bhMP(x,[1,0,0,1]), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
bh1110 = fmincon(@(x) bhMP(x,[1,1,1,0]), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
bh1111 = fmincon(@(x) bhMP(x,[1,1,1,1]), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
bh2111 = fmincon(@(x) bhMP(x,[2,1,1,1]), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
bh1211 = fmincon(@(x) bhMP(x,[1,2,1,1]), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
bh1121 = fmincon(@(x) bhMP(x,[1,1,2,1]), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
bh1112 = fmincon(@(x) bhMP(x,[1,1,1,2]), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)
bh1122 = fmincon(@(x) bhMP(x,[1,1,2,2]), ones(b, 1)/b, -eye(b), zeros(b,1), ones(1,b), 1, [],[],[], options)

bhMP(bhMean, [1,0,0,0])
bhMP(bhVar, [0,1,0,0])
bhMP(bhSkew, [0,0,1,0])
bhMP(bhKurt, [0,0,0,1])
bhMP(bh1100, [1,1,0,0])
bhMP(bh1010, [1,0,1,0])
bhMP(bh1001, [1,0,0,1])
bhMP(bh1110, [1,1,1,0])
bhMP(bh1111, [1,1,1,1])
bhMP(bh2111, [2,1,1,1])
bhMP(bh1211, [1,2,1,1])
bhMP(bh1121, [1,1,2,1])
bhMP(bh1112, [1,1,1,2])
bhMP(bh1122, [1,1,2,2])

bhB4Rets1000 = bhMP(bhWB4, [1,0,0,0])
bhB4Rets0100 = bhMP(bhWB4, [0,1,0,0])
bhB4Rets0010 = bhMP(bhWB4, [0,0,1,0])
bhB4Rets0001 = bhMP(bhWB4, [0,0,0,1])
bhB4Rets1100 = bhMP(bhWB4, [1,1,0,0])
bhB4Rets1010 = bhMP(bhWB4, [1,0,1,0])
bhB4Rets1001 = bhMP(bhWB4, [1,0,0,1])
bhB4Rets1110 = bhMP(bhWB4, [1,1,1,0])
bhB4Rets1111 = bhMP(bhWB4, [1,1,1,1])
bhB4Rets2111 = bhMP(bhWB4, [2,1,1,1])
bhB4Rets1211 = bhMP(bhWB4, [1,2,1,1])
bhB4Rets1121 = bhMP(bhWB4, [1,1,2,1])
bhB4Rets1112 = bhMP(bhWB4, [1,1,1,2])
bhB4Rets1122 = bhMP(bhWB4, [1,1,2,2])

bhRets1000 = bhMP(bhW, [1,0,0,0])
bhRets0100 = bhMP(bhW, [0,1,0,0])
bhRets0010 = bhMP(bhW, [0,0,1,0])
bhRets0001 = bhMP(bhW, [0,0,0,1])
bhRets1100 = bhMP(bhW, [1,1,0,0])
bhRets1010 = bhMP(bhW, [1,0,1,0])
bhRets1001 = bhMP(bhW, [1,0,0,1])
bhRets1110 = bhMP(bhW, [1,1,1,0])
bhRets1111 = bhMP(bhW, [1,1,1,1])
bhRets2111 = bhMP(bhW, [2,1,1,1])
bhRets1211 = bhMP(bhW, [1,2,1,1])
bhRets1121 = bhMP(bhW, [1,1,2,1])
bhRets1112 = bhMP(bhW, [1,1,1,2])
bhRets1122 = bhMP(bhW, [1,1,2,2])