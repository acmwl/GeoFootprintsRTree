#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#define DIMENSIONS 4

void reversebuf(float **buf, int numbuffered)
{
	float *temp;
	
	for(int i=0; i<numbuffered/2; i++) {
		temp = buf[i];
		buf[i] = buf[numbuffered-1-i];
		buf[numbuffered-1-i] = temp;
	}
}

// read and process trajectory data from file 
int readData(FILE *f, int *numusers, int *numdays, float epsilon, int tau)
{
    int i,j,k;
    
    char *line = NULL; // used for fileread
	size_t len = 0; // used for fileread
	ssize_t read; // used for fileread
	const char delim[] = " "; // used for fileread
	char *token; // used for fileread
		
	// Read trajectories from file
	int curuser = -1;
	int curday = 1;
	int curcount = 0;
	int curst; int curend;
	float curmbr[2*DIMENSIONS];
	float **buf;
	float **newbuf;
	int user;
	char day[15] = " ";
	float x,y;
	int numbuffered;
	int numnewbuffered;
	buf = (float **)malloc(tau*sizeof(float *));
	newbuf = (float **)malloc(tau*sizeof(float *));
	for(i=0;i<tau;i++) {
		buf[i] = (float *)malloc(DIMENSIONS*sizeof(float));
		newbuf[i] = (float *)malloc(DIMENSIONS*sizeof(float));
	}
	*numusers = 0;
	*numdays = 0;
	while ((read = getline(&line,&len,f)) != -1)	{
		token = strtok(line,delim);
		user = atoi(token);
		token = strtok(NULL,delim);
		// strncpy(day, token, 14);
		token = strtok(NULL,delim);
		x = atof(token);
		token = strtok(NULL,delim);
		y = atof(token);
		
		// printf("trans: %d %s %f %f\n", user,day,x,y);
		
		if (user>curuser) {
			//"close" previous MBR
			if (curcount>=tau)
				// printf("%d %d %d %d %d %.6f %.6f %.6f %.6f\n", 
					// curuser,curday,curcount,curst,curend,curmbr[0],curmbr[1],curmbr[2],curmbr[3]);
			// initialize new user
			(*numusers)++;
			curuser = user;
			// strcpy(curday, day);
			curmbr[0] = x; curmbr[1] = x; 
			curmbr[2] = y; curmbr[3] = y; 
			curcount = 1; // counts the number of locations in current MBR 
			numbuffered=1;
			buf[0][0]=x; buf[0][1]=y; 
			curst = 0; curend = 0;
		}
		// else if (day>curday) {
		// 	//"close" previous MBR
		// 	if (curcount>=tau)
		// 		printf("%d %d %d %d %d %.3f %.3f %.3f %.3f\n", 
		// 			curuser,curday,curcount,curst,curend,curmbr[0],curmbr[1],curmbr[2],curmbr[3]);
		// 	// initialize new day
		// 	strcpy(curday, day);
		// 	if ((*numdays)<day+1) 
		// 		(*numdays) = day+1;
		// 	curmbr[0] = x; curmbr[1] = x; 
		// 	curmbr[2] = y; curmbr[3] = y; 
		// 	curcount = 1;
		// 	numbuffered=1;
		// 	buf[0][0]=x; buf[0][1]=y;
		// 	curst = 0; curend = 0;
		// }
		else {
			if (x-curmbr[0] > epsilon || curmbr[1]-x > epsilon ||
			    y-curmbr[2] > epsilon || curmbr[3]-y > epsilon)
			{
				//"close" previous MBR
				if (curcount>=tau) {
					// printf("%d %d %d %d %d %.6f %.6f %.6f %.6f\n", 
					// curuser,curday,curcount,curst,curend,curmbr[0],curmbr[1],curmbr[2],curmbr[3]);
					//start new MBR
					curmbr[0] = x; curmbr[1] = x; 
					curmbr[2] = y; curmbr[3] = y; 
					curcount = 1;
					numbuffered=1;
					buf[0][0]=x; buf[0][1]=y;
					curend++; curst = curend; 
				}
				else {
					//start new MBR, add as many past points as possible
					curmbr[0] = x; curmbr[1] = x; 
					curmbr[2] = y; curmbr[3] = y; 
					curcount = 1;
					numnewbuffered=0;
					curend++; curst = curend;
					//printf("debug1: %f numbuf=%d\n",x,numbuffered);
					for (i=numbuffered-1;i>=0; i--) {
						if (buf[i][0]-curmbr[0] > epsilon || curmbr[1]-buf[i][0] > epsilon ||
							buf[i][1]-curmbr[2] > epsilon || curmbr[3]-buf[i][1] > epsilon)
								break;
						// not inserted correctly; should go to position 0
						newbuf[numnewbuffered][0] = buf[i][0];
						newbuf[numnewbuffered++][1] = buf[i][1];
						curcount++;
						curst--;
						float xx = buf[i][0]; 
						float yy = buf[i][1];
						if (curmbr[0]>xx) curmbr[0]=xx;
						if (curmbr[1]>yy) curmbr[1]=yy;
						if (curmbr[2]<xx) curmbr[2]=xx;
						if (curmbr[3]<yy) curmbr[3]=yy;
					}
					reversebuf(newbuf,numnewbuffered);
					newbuf[numnewbuffered][0]=x; 
					newbuf[numnewbuffered++][1]=y;

					for (i=0;i<numnewbuffered;i++) {
						buf[i][0] = newbuf[i][0];
						buf[i][1] = newbuf[i][1];
					}
					numbuffered = numnewbuffered;
					//printf("debug: %f numbuf=%d %.3f %.3f %.3f %.3f\n",buf[0][0],numbuffered,curmbr[0],curmbr[1],curmbr[2],curmbr[3]);
				}
			}
			else {
				//expand current mbr
				if (curmbr[0]>x) curmbr[0]=x;
				if (curmbr[1]>y) curmbr[1]=y;
				if (curmbr[2]<x) curmbr[2]=x;
				if (curmbr[3]<y) curmbr[3]=y;
				curcount++;
				curend++;
				if (numbuffered<tau) {
					buf[numbuffered][0] = x;
					buf[numbuffered++][1] = y;
				}
			}
		}
	}
	//"close" last MBR
	if (curcount>=tau) {
		// printf("%d %d %d %d %d %.6f %.6f %.6f %.6f\n", 
		// curuser,curday,curcount,curst,curend,curmbr[0],curmbr[1],curmbr[2],curmbr[3]);
	}
		
	for(i=0;i<tau;i++) {
		free(buf[i]);
		free(newbuf[i]);
	}
	free(buf);
	free(newbuf);

    return 0;
}


int main(int argc, char **argv)
{
    int numusers,numdays;
	FILE *f; // graph input file
	
    clock_t t;
    double time_taken;
    
    if (argc != 4) {
    	printf("arguments: <data file> <epsilon> <tau>\n");
    	return -1;
    }
    
    f = fopen(argv[1],"r");
    
    float epsilon = atof(argv[2]);
    int tau = atoi(argv[3]);    
	t = clock();
	if ((readData(f, &numusers, &numdays, epsilon, tau)))
	{
		printf("something went wrong while reading data file\n");
    	return -1;
    }
	t = clock() - t;
	time_taken = ((double)t)/CLOCKS_PER_SEC;
	printf("Completed in %f\n", time_taken);
    
	return 0;
}
