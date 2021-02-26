# SpinalHDL ResNet20 AdderNet

这是一个使用 SpinalHDL 实现的 AdderNet 版的 ResNet20。数据集为 cifar10，采用华为开源的网络及权重。

目前已完成的仿真结果：303/333

代码还有不少优化空间，比如量化策略，我看华为的论文里好像 8 比特就够了，怎么实现的还没细看。还有一个是存储器的问题，我这个结构可能 bram 用的多，但是有很多地方存储器的深度可以减少的。

文档还需要补全

## spinalHDL 简介

一种基于 scala 构建的 rtl 级的硬件描述语言。相比 Verilog/VHDL，除了些许语法的细微不同外，我认为主要区别有两点：1.Verilog 中的 wire 和 reg 是一个比较失败的设计，实际上 EDA 还是根据代码去推断该信号是 wire 或 reg。spinalHDL则是仅需要在信号定义时设定 wire 或 reg，代码中不需要额外写 `always @ (posedge clk) begin` 等内容，生成 verilog 时会根据信号的定义自动帮你补上。2.Verilog 的参数化能力很弱，spinalHDL 强化了参数化的能力和抽象能力。举几个例子，对于抽象能力，可能某个工程中大量运用了一组很规则的信号（例如 AXI），在写 Verilog 的时候，你可能需要反复写这一组十几个信号赋给另一组，赋一次值就要写个十几行。而在 spinalHDL 中你可以把这组信号定义为一个 `Bundle` 这样赋值的时候只需要写一行就行。对于参数化能力，举个例子你可能会需要写下面这样一堆东西：

```verilog
data_out_0 <= data_in_0
data_out_1 <= data_in_0 + data_in_1
data_out_2 <= data_in_2
data_out_3 <= data_in_2 + data_in_3
...
```

verilog 没有很灵活的参数化的描述能力，你需要几路就手动写几行代码。此时你可能会想到可以用 python 或者其他这类高级语言用 print 去打印出来

```python
repeat = 3
for i in range(0,repeat):
    if mod(i,2) == 0:
        print("data_out_%d <= data_in_%d",i,i)
    else:
        print("data_out_%d <= data_in_%d + data_in_%d",i,i,i+1)
```

然而使用 SpinalHDL 时

```scala
val repeat = 3
for(i <- 0 until 3) {
  if(i%2 == 0) {
    data_out(i) := data_in(i)
  }else {
    data_out(i) := data_in(i) + data_in(i+1)
  }
}
```

以上只是一个最简单的例子，是 python 等其他语言和 SpinalHDL 差距最小的情况，依然能够看出 SpinalHDL 的写法思路更加简单清晰。复杂的例子看本项目的代码就好啦，可以十分明显的看出，SpinalHDL 的设计思路与 verilog 完全一致，但代码可读性、参数化能力大幅领先。也就是，用硬件的方式去设计和思考，同时学习软件中优秀的代码风格。

## AdderNet 简介

看华为的论文和代码吧

开始写这个代码的时候我还不知道他们一个月前新发了一篇用 FPGA 实现 addernet 的论文，有机会拜读一下

## 代码结构

代码主要在 `src` 文件夹中。该文件夹内主要包含以下三个部分内容：

* mylib：这个文件夹放的主要是一些可以作为库来调用的文件

* src：本项目核心的代码内容。其中包含 `NN` 文件夹和另外三个文件。 `NN` 文件夹内为我设计的神经网络通用的模块，外面的文件则是针对本项目要实现的 `resnet20` 特别写的一些代码。顶层文件为 `ResNet20.scala`

* test：主要是各种仿真文件。

### mylib

有空再继续写
