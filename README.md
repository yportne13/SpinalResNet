# SpinalHDL ResNet20 AdderNet

这是一个使用 SpinalHDL 实现的 AdderNet 版的 ResNet20。数据集为 cifar10，采用华为开源的网络及权重。

## spinalHDL 简介

一种基于 scala 构建的 rtl 级的硬件描述语言。相比 Verilog/VHDL，除了些许语法的细微不同外，我认为主要区别有两点：1.Verilog 中的 wire 和 reg 是一个比较失败的设计，实际上 EDA 还是根据代码去推断该信号是 wire 或 reg。spinalHDL则是仅需要在信号定义时设定 wire 或 reg，代码中不需要额外写 `always @ (posedge clk) begin` 等内容，生成 verilog 时会根据信号的定义自动帮你补上。2.Verilog 的参数化能力很弱，spinalHDL 强化了参数化的能力和抽象能力。举几个例子，对于抽象能力，可能某个工程中大量运用了一组很规则的信号（例如 AXI），在写 Verilog 的时候，你可能需要反复写这一组十几个信号赋给另一组，赋一次值就要写个十几行。而在 spinalHDL 中你可以把这组信号定义为一个 `Bundle` 这样赋值的时候只需要写一行就行。对于参数化能力，举个例子你可能会需要写下面这样一堆东西：

```verilog
data_out_0 <= data_in_0
data_out_1 <= data_in_0 + data_in_1
data_out_2 <= data_in_2
data_out_3 <= data_in_2 + data_in_3
...
```

此时你可能会想到可以用 python 或者之类的语言用 print 去打印出来

```python
repeat = 
```

## AdderNet 简介

看华为的论文和代码吧

## 代码结构

代码包含 GoldenModel
