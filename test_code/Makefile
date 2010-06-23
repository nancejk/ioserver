IOSBASE := /home/slowcontroller/ios_extended
IOSLIBDIR := $(IOSBASE)/devios
TARGET := read_data320
CXX := g++
READSRC := read.cxx
IOSLIBS := ioscarrier
IOSCARRIERINC := ioscarrier 
IOSSTUFF := ioscarrier.o ainmc320.o ainsc320.o mccd320.o sccd320.o rsts320.o
IOSTGTS := $(addprefix $(IOSBASE)/ios320/, $(IOSSTUFF)) 

$(TARGET): $(READSRC)
	$(CXX) -c -o $(TARGET).o $(READSRC) -I$(IOSBASE)
	$(CXX) -o $(TARGET) $(TARGET).o $(IOSTGTS)
