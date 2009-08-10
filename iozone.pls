#proc getdata
file: @file
fieldnames: block read write rread rwrite
delim: comma
nfields: 5
showdata: yes

#proc areadef
      title: @title
      titledetails: align=C size=13 adjust=0,0.2
      xautorange: datafield=1 lowfix=4096
#if @iops = 0      
      yautorange: datafield=2,3,4,5
#else
      yautorange: datafield=4,5
#endif
      xscaletype: log     

#if @iops = 0
#proc lineplot
      xfield: block
      yfield: read
      linedetails: color=blue
      legendlabel: Read

#proc lineplot
      xfield: block
      yfield: write
      linedetails: color=red
      legendlabel: Write
#endif

#proc lineplot
      xfield: block
      yfield: rread
      linedetails: color=green
      legendlabel: Random read

#proc lineplot
      xfield: block
      yfield: rwrite
      linedetails: color=lightpurple
      legendlabel: Random write

#proc legend
      location: max-0.8 max+0.4

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
