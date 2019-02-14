# Trust Model Simulator
Cody Lewis, supervised by Dr Nan Li

A simulator of an IoT trust model based on http://people.cs.vt.edu/~irchen/5984/pdf/Saied-CS14.pdf
used to show the effects of context based attacks on the system.

## Requirements
  - R

## Configuration
Get the required packages by running
```
Rscript src/TrustModel/requirements.r
```

## Running
```
cd src/TrustModel/ && Rscript ConsoleInterface.r
```
Which will give some help documentation, telling you how to run the program
completely

Or you can run the web-based graphical interface with

```
cd src/TrustModel && Rscript WebServer.r
```
