% Import data first from smry.csv
% First with kurtosis as marker size
figure
scatter3(smry(:,1), smry(:,2), smry(:,3), smry(:,4))
xlabel('Mean')
ylabel('Variance')
zlabel('Skew')

% With squared mean as marker size
figure
scatter3(smry(:,2), smry(:,3), smry(:,4), 10^6*smry(:,1).^2)
xlabel('Variance')
ylabel('Skew')
zlabel('Kurtosis')

% 2D scatters
figure
scatter(smry(:,2), smry(:,1))
xlabel('Variance')
ylabel('Mean')

figure
scatter(smry(:,3), smry(:,1))
xlabel('Skew')
ylabel('Mean')

figure
scatter(smry(:,4), smry(:,1))
xlabel('Kurtosis')
ylabel('Mean')
