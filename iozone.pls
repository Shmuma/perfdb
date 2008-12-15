#proc getdata
data:
4096,797.2021,381.8623,1.3340,33.7793,
8192,807.2686,427.0586,2.6025,66.2617,
16384,812.5879,448.2656,4.9941,122.0684,
32768,813.1680,452.2188,9.2188,189.1807,
65536,814.6250,450.4912,16.1934,147.9346,
131072,820.3916,479.8691,26.9307,109.4248,
262144,814.9795,477.7813,42.7725,97.1133,
524288,825.8701,491.5898,70.0742,103.6201,
1048576,826.4990,424.5078,114.5010,129.5986,
2097152,803.1885,454.2832,139.3809,172.2412,
4194304,702.1758,402.6465,184.3594,215.6660,
8388608,700.2646,414.4443,298.5586,255.9834,
16777216,742.7402,417.9248,433.1387,255.4795,

fieldnames: block read write rread rwrite

delim: comma
nfields: 5

#proc areadef
      title: NDA2 test results, JBOD, 16 disks
      titledetails: align=C size=13 adjust=0,0.2
      xautorange: datafield=1 lowfix=4096
      yautorange: datafield=2,3,4,5
      xscaletype: log     

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
      stubs: inc 100
      label: Throughput, MB/s
      labeldetails: adjust=-0.1,0
      grid: color=gray(0.8) style=1
