# 10x10
plot.new()
plot.window(xlim = c(0, 55), ylim = c(0, 55))
segments(x0 = seq(from = 5, to = 55, by = 5), x1 = seq(from = 5, to = 55, by = 5), 
         y0 = 0, y1 = 100)
segments(x0 = 0, x1 = 100, 
         y0 = seq(from = 5, to = 55, by = 5), y1 = seq(from = 5, to = 55, by = 5))
text(x = seq(from = 7, to = 55, by = 5), y = 8, label = '1':'10')
text(x = seq(from = 7, to = 55, by = 5), y = 13, label = '20':'11')
text(x = seq(from = 7, to = 55, by = 5), y = 18, label = '21':'30')
text(x = seq(from = 7, to = 55, by = 5), y = 23, label = '40':'31')
text(x = seq(from = 7, to = 55, by = 5), y = 28, label = '41':'50')
text(x = seq(from = 7, to = 55, by = 5), y = 33, label = '60':'51')
text(x = seq(from = 7, to = 55, by = 5), y = 38, label = '61':'70')
text(x = seq(from = 7, to = 55, by = 5), y = 43, label = '80':'71')
text(x = seq(from = 7, to = 55, by = 5), y = 48, label = '81':'90')
text(x = seq(from = 7, to = 55, by = 5), y = 53, label = '100':'91')

arrows(x0 = 5, x1 = 19, 
       y0 = 4, y1 = 25, col = 'green', lwd = 2)
arrows(x0 = 20, x1 = 38, 
       y0 = 5, y1 = 14, col = 'green', lwd = 2)
arrows(x0 = 48, x1 = 52, 
       y0 = 5, y1 = 25, col = 'green', lwd = 2)
arrows(x0 = 5, x1 = 14, 
       y0 = 15, y1 = 30, col = 'green', lwd = 2)
arrows(x0 = 45, x1 = 24, 
       y0 = 15, y1 = 48, col = 'green', lwd = 2)
arrows(x0 = 30, x1 = 23, 
       y0 = 20, y1 = 28, col = 'green', lwd = 2)
arrows(x0 = 55, x1 = 37, 
       y0 = 30, y1 = 37, col = 'green', lwd = 2)
arrows(x0 = 53, x1 = 53, 
       y0 = 40, y1 = 55, col = 'green', lwd = 2)
arrows(x0 = 7, x1 = 7, 
       y0 = 42, y1 = 53, col = 'green', lwd = 2)

arrows(x0 = 18, x1 = 18, 
       y0 = 53, y1 = 40, col = 'red', lwd = 2)
arrows(x0 = 33, x1 = 33, 
       y0 = 53, y1 = 40, col = 'red', lwd = 2)
arrows(x0 = 43, x1 = 43, 
       y0 = 53, y1 = 40, col = 'red', lwd = 2)
arrows(x0 = 23, x1 = 5, 
       y0 = 38, y1 = 33, col = 'red', lwd = 2)
arrows(x0 = 13, x1 = 13, 
       y0 = 38, y1 = 12, col = 'red', lwd = 2)
arrows(x0 = 25, x1 = 43, 
       y0 = 33, y1 = 33, col = 'red', lwd = 2)
arrows(x0 = 48, x1 = 54, 
       y0 = 30, y1 = 10, col = 'red', lwd = 2)
arrows(x0 = 38, x1 = 33, 
       y0 = 30, y1 = 17, col = 'red', lwd = 2)
arrows(x0 = 28, x1 = 33, 
       y0 = 14, y1 = 8, col = 'red', lwd = 2)











# 7x6
plot.new()
plot.window(xlim = c(0, 50), ylim = c(0, 50))
segments(x0 = seq(from = 5, to = 35, by = 5), x1 = seq(from = 5, to = 35, by = 5), 
         y0 = 0, y1 = 50)
segments(x0 = 0, x1 = 50, 
         y0 = seq(from = 5, to = 40, by = 5), y1 = seq(from = 5, to = 40, by = 5))
text(x = seq(from = 7, to = 35, by = 5), y = 8, label = '1':'6')
text(x = seq(from = 7, to = 35, by = 5), y = 13, label = '12':'7')
text(x = seq(from = 7, to = 35, by = 5), y = 18, label = '13':'18')
text(x = seq(from = 7, to = 35, by = 5), y = 23, label = '24':'19')
text(x = seq(from = 7, to = 35, by = 5), y = 28, label = '25':'30')
text(x = seq(from = 7, to = 35, by = 5), y = 33, label = '36':'31')
text(x = seq(from = 7, to = 35, by = 5), y = 38, label = '37':'42')

arrows(x0 = 5, x1 = 13, 
       y0 = 5, y1 = 23, col = 'green', lwd = 2)
arrows(x0 = 23, x1 = 18, 
       y0 = 10, y1 = 27, col = 'green', lwd = 2)
arrows(x0 = 27, x1 = 33, 
       y0 = 22, y1 = 32, col = 'green', lwd = 2)
arrows(x0 = 22, x1 = 27, 
       y0 = 30, y1 = 38, col = 'green', lwd = 2)

arrows(x0 = 22, x1 = 32, 
       y0 = 18, y1 = 7, col = 'red', lwd = 2)
arrows(x0 = 33, x1 = 33, 
       y0 = 30, y1 = 17, col = 'red', lwd = 2)
arrows(x0 = 12, x1 = 18, 
       y0 = 33, y1 = 33, col = 'red', lwd = 2)
arrows(x0 = 23, x1 = 12, 
       y0 = 38, y1 = 28, col = 'red', lwd = 2)
