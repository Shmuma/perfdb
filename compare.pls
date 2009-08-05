#proc getdata
file: @file
fieldnames: block dat1 dat2 dat3 dat4 dat5 dat6 dat7 dat8 dat9 dat10
delim: comma
nfields: @fields
showdata: yes

#proc areadef
      title: @title
      titledetails: align=C size=13 adjust=0,0.2
      xautorange: datafield=1 lowfix=4096
      yautorange: datafield=2,3,4,5,6,7,8,9,10,11
      xscaletype: log     

#proc lineplot
      xfield: block
      yfield: dat1
      linedetails: color=blue
      legendlabel: @label1

#proc lineplot
      xfield: block
      yfield: dat2
      linedetails: color=red
      legendlabel: @label2

#proc lineplot
      xfield: block
      yfield: dat3
      linedetails: color=green
      legendlabel: @label3

#proc lineplot
      xfield: block
      yfield: dat4
      linedetails: color=purple
      legendlabel: @label4

#proc lineplot
      xfield: block
      yfield: dat5
      linedetails: color=gray(0.3)
      legendlabel: @label5

#proc lineplot
      xfield: block
      yfield: dat6
      linedetails: color=magenta
      legendlabel: @label6

#proc lineplot
      xfield: block
      yfield: dat7
      linedetails: color=yellow
      legendlabel: @label7

#proc lineplot
      xfield: block
      yfield: dat8
      linedetails: color=oceanblue
      legendlabel: @label8

#proc lineplot
      xfield: block
      yfield: dat9
      linedetails: color=claret
      legendlabel: @label9

#proc lineplot
      xfield: block
      yfield: dat10
      linedetails: color=orange
      legendlabel: @label10

#proc legend
      location: max-1 max+0

#proc xaxis
      axisline: width=0.7 color=black
      selflocatingstubs: text
4096 4K
8192 8K
16384 16K
32768 32K
65536 64K
131072 128K
262144 256K
524288 512K
1048576 1M
2097152 2M
4194304 4M
8388608 8M
16777216 16M

      label: Block size
      grid: color=gray(0.8) style=1

#proc yaxis
      axisline: width=0.7 color=black
      stubs: inc
      label: @yaxis
      labeldetails: adjust=-0.2,0
      grid: color=gray(0.8) style=1
