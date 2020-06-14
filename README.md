# Critical-Path-Scheduling
Implemented Critical Path Scheduling algorithm in Prolog

## Description

![CPS-example](https://github.com/Saurabh110/Critical-Path-Scheduling/blob/master/CriticalPath-example.jpg)  
The critical path method (CPM), or critical path analysis (CPA), is an algorithm for scheduling a set of project activities. A critical path is determined by identifying the longest stretch of dependent activities and measuring the time required to complete them from start to finish.


## Implemantation

```criticalPath(<task>, <path>)```  
If the task is mentioned and path is not mentioned, then this function will return 	the critical path of that task. If critical path for that function is not present then it will return an empty list.

```earlyFinish(<task>, <time>)```  
This function can map the earlyFinish time for a given task or it can map to true	or false if given time is earlyFinish time for the given task.

```lateStart(<task>, <time>)```  
This function can map the lateStart time for a given task or it can map to true	or false if given time is lateStart time for the given task.

```maxSlack(<task>, <time>)```  
This function maps to true only if the passed time is the Maximum Slack of the whole graph and that slack time is of the passed task.
