#include <sixense.h>
#include <stdlib.h>
#include <stdio.h>

// compile: gcc test.c -lsixense_x64
int main() {
    printf("Hello, world!\n");
    sixenseInit();
    sleep(2);
    int maxB = sixenseGetMaxBases();
    sixenseSetActiveBase(0);
    sixenseAutoEnableHemisphereTracking(0);
    sixenseSetActiveBase(0);
    int conn0 = sixenseIsBaseConnected(0);
    int conn1 = sixenseIsBaseConnected(1);
    sixenseSetActiveBase(0);
    int conn0b = sixenseIsBaseConnected(0);
    int maxCon = sixenseGetMaxControllers();
    int e0 = sixenseIsControllerEnabled(0);
    sixenseControllerData dat;
    printf("Active: %d, conn: %d %d %d, maxContr: %d, e0 enabled: %d, act: %d\n", maxB, conn0, conn0b, conn1, maxCon, e0, sixenseGetNumActiveControllers());
    
    while(1) {
        sixenseGetNewestData(0, &dat);
        printf("Pos: %f %f %f\n", dat.pos[0], dat.pos[1], dat.pos[2]);
        usleep(1000000 / 128.0f);
    }
    sixenseExit();
}
