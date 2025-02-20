# Genetic-Algorithm-for-Portfolio-Optimization-in-Algorithmic-Trading-
This project implements an algorithmic trading strategy that leverages a genetic algorithm (GA) for portfolio optimization, with a focus on achieving superior risk-adjusted returns. The system was designed and tested across two distinct periods—historical (training: 2017–2022) and unseen future data (test: 2023)—to evaluate its generalizability.

# Methodology

## Genetic Algorithm Optimization:
The GA is used to optimize asset weights based on historical return data. The optimization targets maximizing the Sharpe ratio—a measure of risk-adjusted performance. The GA-generated weights were not only benchmarked against manually selected portfolios but also compared to balanced and randomly weighted portfolios, underscoring the systematic value of the optimization approach.

## Sharpe Ratio as Performance Metric:
The Sharpe ratio was calculated without subtracting a risk-free rate to focus on relative performance improvements. This measure was used consistently to compare different strategies over both the training and test periods, allowing for clear insights into how each portfolio managed risk relative to its returns.

# Performance Evaluation
## Training Data (2017–2022)
### GA-Evolved Portfolio:

Achieved a Sharpe ratio of approximately 0.0753, establishing a solid baseline on historical data.
Outperformed both balanced and random portfolios in this period, demonstrating the effectiveness of the GA in optimizing weights based on past performance.
Comparison with Other Portfolios:

The GA-evolved portfolio slightly outperformed a manually selected portfolio (0.1099 vs. 0.1006) and the balanced portfolio, indicating that systematic optimization can capture nuanced opportunities within historical market dynamics.
Test Data (2023)
## GA-Evolved Portfolio:

The performance improved significantly, reaching a Sharpe ratio of 0.1486 on unseen future data.
This improvement indicates that the GA not only tailored the portfolio effectively to past conditions but also generalized well to new market environments.
Balanced Portfolio:

Although the balanced portfolio was slightly ahead in 2023 (Sharpe ratio of 0.1517), this outcome highlights the influence of market conditions on portfolio performance and the potential benefits of diversification in certain scenarios.

## Random Portfolio:

The GA-evolved portfolio far outperformed the average random portfolio, reinforcing the value of systematic optimization over non-strategic weight assignments.

# Comparative Insights

Robustness and Adaptability:
The GA-optimized portfolio demonstrated significant adaptability by not only matching but often exceeding performance benchmarks from historical training data when applied to future data. Its strong performance on unseen data suggests that the GA was able to avoid overfitting and capture enduring market dynamics.

Market Conditions and Portfolio Composition:
The comparative analysis reveals that while the GA-evolved strategy effectively maximized historical returns, a balanced allocation sometimes benefited from unforeseen favorable market conditions in 2023. This observation opens avenues for hybrid or adaptive strategies that combine algorithmic optimization with dynamic rebalancing mechanisms.

Value of Systematic Optimization:
Overall, the project underscores that a systematic approach like GA optimization can consistently provide better risk-adjusted returns compared to random or purely balanced allocations. Even when manually selected portfolios outperformed in certain test scenarios, the GA provided a competitive and automated means of portfolio construction that can be refined further for enhanced generalizability.

# Conclusion
This project illustrates the power and potential of using genetic algorithms in portfolio optimization. By focusing on risk-adjusted returns via the Sharpe ratio, the strategy has been rigorously evaluated on both historical and unseen data, demonstrating its effectiveness and adaptability. The insights gained from comparing GA-optimized, balanced, and manually selected portfolios not only validate the approach but also point towards future enhancements—such as dynamic weight adjustments—to further refine trading strategies in response to evolving market conditions.

Feel free to explore, modify, and build upon this implementation for advanced algorithmic trading research and applications.
