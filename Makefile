OUT = npc

all: $(OUT)

$(OUT): Npc.hs PacketParser.hs Constants.hs
	ghc --make -O2 -o $(OUT) Npc.hs

clean:
	rm $(OUT) Npc.hi Npc.o PacketParser.hi PacketParser.o Constants.hi Constants.o