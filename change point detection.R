# 加载 readxl 包
library(readxl)

# 读取数据
data <- read_excel("C:/Users/IKUN/Desktop/dataproject.xlsx", col_names = FALSE)
data <- as.numeric(data[[1]])

# 检查数据
print(data)

# 参数设定
n <- length(data)
alpha <- 0.05
mu <- mean(data)

# 定义 Z(theta) 计算函数
Z <- function(theta) {
  if (theta == 0 || theta == n) {
    return(NA)
  }
  sum(data[(theta+1):n] - mu) / sqrt(n - theta)
}

# 计算 Z(theta) 的值
Z_values <- sapply(1:(n-1), Z)

# 查找最大 Z(theta) 和临界值
Z_max <- max(Z_values, na.rm = TRUE)
c <- qnorm(1 - alpha)

# 判断是否存在变化点
change_point_exists <- Z_max > c

# 输出结果
result <- list(
  Z_values = Z_values,
  Z_max = Z_max,
  critical_value = c,
  change_point_exists = change_point_exists
)

print(result)

# 设置 y 轴范围为 (-50, 300)
y_min <- -50
y_max <- 300

# 绘图
plot(1:(n-1), Z_values, type = 'o', xlab = 'Theta', ylab = 'Z(theta)', main = 'Z(theta) Values for Change Point Detection', ylim = c(y_min, y_max))
abline(h = c, col = 'red', lty = 2)
legend('topright', legend = sprintf('Critical Value (c) = %.2f', c), col = 'red', lty = 2)
