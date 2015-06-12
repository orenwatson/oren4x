/* This is the version with the horrifying scrolling interpolating code. Welcome to DIE!
(to disable scrolling intepolation comment the next line)*/
#include "stdlib.h"
#define MUHUHAHAHA

#ifdef MUHUHAHAHA
#define N_PREVFRAMES 21
static int thisframen;//which index to save this frame to.
static unsigned *prevframe[N_PREVFRAMES];//saved frames in 4 bytes hash of 9 surrounding pixels, for each pixel, in each frame.
static unsigned *curframebuf;//save the preliminary render (noscroll) of current frame here
static void find_scroll_direction(unsigned &direction,unsigned &f0,unsigned &f1,int Xres,int Yres,int i,int j);
static const int dir_right = 1;
static const int dir_left = 2;
static const int dir_down = 4;
static const int dir_up = 8;
#endif

#define ei else if
void oren4x( unsigned char * pIn, unsigned char * pOut, int Xres, int Yres, int srcBpL, int BpL){
	int i,j,k;
/*-2 -1  0  1  2
   A  B  C  D  E -2
   F  G  H  I  J -1
   K  L  M  N  O  0
   P  Q  R  S  T  1
   U  V  W  X  Y  2 */
	unsigned iA,iB,iC,iD,iE,iF,iG,iH,iI,iJ,iK,iL,iM,iN,iO,iP,iQ,iR,iS,iT,iU,iV,iW,iX,iY;
#ifdef MUHUHAHAHA
	if(!curframebuf)curframebuf=(unsigned *)malloc(Xres*4*Yres*4*sizeof(unsigned));
	if(!prevframe[thisframen])prevframe[thisframen]=(unsigned *)malloc(Xres*Yres*sizeof(unsigned));
#endif
	for(j=0;j<Yres;j++){
		for(i=0;i<Xres;i++){
/* macros for safe reading of neighbouring pixels */
#define in(I,J) (*(unsigned*)(pIn+(i+I)*4+(j+J)*srcBpL))
#define vl(I,J) (i+I>=0&&i+I<Xres&&j+J>=0&&j+J<Yres)
#define vg(I,J,D) (vl(I,J)?in(I,J):D)
/* read a 5x5 block around this pixel */
			iM = in( 0, 0);
			iH = vg( 0,-1,iM);
			iL = vg(-1, 0,iM);
			iR = vg( 0, 1,iM);
			iN = vg( 1, 0,iM);
			iG = vg(-1,-1,vg(-1, 0,vg(0,-1,iM)));
			iI = vg( 1,-1,vg( 1, 0,vg(0,-1,iM)));
			iQ = vg(-1, 1,vg(-1, 0,vg(0, 1,iM)));
			iS = vg( 1, 1,vg( 1, 0,vg(0, 1,iM)));
			iF = vg(-2,-1,iG);
			iA = vg(-2,-2,iG);
			iB = vg(-1,-2,iG);
			iC = vg( 0,-2,iH);
			iD = vg( 1,-2,iI);
			iE = vg( 2,-2,iI);
			iJ = vg( 2,-1,iI);
			iO = vg( 2, 0,iN);
			iT = vg( 2, 1,iS);
			iY = vg( 2, 2,iS);
			iX = vg( 1, 2,iS);
			iW = vg( 0, 2,iR);
			iV = vg(-1, 2,iQ);
			iU = vg(-2, 2,iQ);
			iP = vg(-2, 1,iQ);
			iK = vg(-2, 0,iL);
#ifdef MUHUHAHAHA
			/* a hash of the 3x3 block around this pixel, used for scroll detection */
			unsigned curhash =
				25911*iG^25193*iH^26019*iI^
				26331*iL^26147*iM^26571*iN^
				17771*iQ^17183*iR^11787*iS;
			prevframe[thisframen][i+j*Xres]=curhash;
#define p(I,J)	(curframebuf[i*4+I+(j*4+J)*Xres*4])
#else
#define p(I,J)	(*(unsigned*)(pOut+(i*4+I)*4+(j*4+J)*BpL))
#endif
#define o(I,J)	(*(unsigned*)(pOut+(i*4+I)*4+(j*4+J)*BpL))
/* match against a pattern of 5x5 pixels */
#define pat(A_,B_,C_,D_,E_,F_,G_,H_,I_,J_,K_,L_,M_,N_,O_,P_,Q_,R_,S_,T_,U_,V_,W_,X_,Y_) \
(A_(riA)&&B_(riB)&&C_(riC)&&D_(riD)&&E_(riE)&&\
 F_(riF)&&G_(riG)&&H_(riH)&&I_(riI)&&J_(riJ)&&\
 K_(riK)&&L_(riL)&&M_(riM)&&N_(riN)&&O_(riO)&&\
 P_(riP)&&Q_(riQ)&&R_(riR)&&S_(riS)&&T_(riT)&&\
 U_(riU)&&V_(riV)&&W_(riW)&&X_(riX)&&Y_(riY))

#define ap(a,b...) a(b)
#define ap2(a,b...) a(b)
#define ap3(a,b...) a(b)
/* used for patterns which don't look at the outer pixels */
#define ap9(f,G_,H_,I_,L_,M_,N_,Q_,R_,S_,b...) \
f(Z,Z ,Z ,Z ,Z,\
  Z,G_,H_,I_,Z,\
  Z,L_,M_,N_,Z,\
  Z,Q_,R_,S_,Z,\
  Z,Z ,Z ,Z ,Z,b)
/* use in a new scope to write the same code and have the pattern rotated, flipped, etc */
#define normal_meanings \
unsigned &riA=iA,&riB=iB,&riC=iC,&riD=iD,&riE=iE, \
         &riF=iF,&riG=iG,&riH=iH,&riI=iI,&riJ=iJ, \
         &riK=iK,&riL=iL,&riM=iM,&riN=iN,&riO=iO, \
         &riP=iP,&riQ=iQ,&riR=iR,&riS=iS,&riT=iT, \
         &riU=iU,&riV=iV,&riW=iW,&riX=iX,&riY=iY
#define rot090_meanings \
unsigned &riA=iE,&riB=iJ,&riC=iO,&riD=iT,&riE=iY, \
         &riF=iD,&riG=iI,&riH=iN,&riI=iS,&riJ=iX, \
         &riK=iC,&riL=iH,&riM=iM,&riN=iR,&riO=iW, \
         &riP=iB,&riQ=iG,&riR=iL,&riS=iQ,&riT=iV, \
         &riU=iA,&riV=iF,&riW=iK,&riX=iP,&riY=iU
#define rot180_meanings \
unsigned &riA=iY,&riB=iX,&riC=iW,&riD=iV,&riE=iU, \
         &riF=iT,&riG=iS,&riH=iR,&riI=iQ,&riJ=iP, \
         &riK=iO,&riL=iN,&riM=iM,&riN=iL,&riO=iK, \
         &riP=iJ,&riQ=iI,&riR=iH,&riS=iG,&riT=iF, \
         &riU=iE,&riV=iD,&riW=iC,&riX=iB,&riY=iA
#define rot270_meanings \
unsigned &riA=iU,&riB=iP,&riC=iK,&riD=iF,&riE=iA, \
         &riF=iV,&riG=iQ,&riH=iL,&riI=iG,&riJ=iB, \
         &riK=iW,&riL=iR,&riM=iM,&riN=iH,&riO=iC, \
         &riP=iX,&riQ=iS,&riR=iN,&riS=iI,&riT=iD, \
         &riU=iY,&riV=iT,&riW=iO,&riX=iJ,&riY=iE
#define fliped_meanings \
unsigned &riA=iU,&riB=iV,&riC=iW,&riD=iX,&riE=iY, \
         &riF=iP,&riG=iQ,&riH=iR,&riI=iS,&riJ=iT, \
         &riK=iK,&riL=iL,&riM=iM,&riN=iN,&riO=iO, \
         &riP=iF,&riQ=iG,&riR=iH,&riS=iI,&riT=iJ, \
         &riU=iA,&riV=iB,&riW=iC,&riX=iD,&riY=iE
#define flp090_meanings \
unsigned &riA=iA,&riB=iF,&riC=iK,&riD=iP,&riE=iU, \
         &riF=iB,&riG=iG,&riH=iL,&riI=iQ,&riJ=iV, \
         &riK=iC,&riL=iH,&riM=iM,&riN=iR,&riO=iW, \
         &riP=iD,&riQ=iI,&riR=iN,&riS=iS,&riT=iX, \
         &riU=iE,&riV=iJ,&riW=iO,&riX=iT,&riY=iY
#define flp180_meanings \
unsigned &riA=iE,&riB=iD,&riC=iC,&riD=iB,&riE=iA, \
         &riF=iJ,&riG=iI,&riH=iH,&riI=iG,&riJ=iF, \
         &riK=iO,&riL=iN,&riM=iM,&riN=iL,&riO=iK, \
         &riP=iT,&riQ=iS,&riR=iR,&riS=iQ,&riT=iP, \
         &riU=iY,&riV=iX,&riW=iW,&riX=iV,&riY=iU
#define flp270_meanings \
unsigned &riA=iY,&riB=iT,&riC=iO,&riD=iJ,&riE=iE, \
         &riF=iX,&riG=iS,&riH=iN,&riI=iI,&riJ=iD, \
         &riK=iW,&riL=iR,&riM=iM,&riN=iH,&riO=iC, \
         &riP=iV,&riQ=iQ,&riR=iL,&riS=iG,&riT=iB, \
         &riU=iU,&riV=iP,&riW=iK,&riX=iF,&riY=iA


#define rot4x4(o1,o2,o3,o4,o5,o6,o7,o8,o9,o10,o11,o12,o13,o14,o15,o16) \
o13,o9,o5,o1,\
o14,o10,o6,o2,\
o15,o11,o7,o3,\
o16,o12,o8,o4
#define flp4x4(o1,o2,o3,o4,o5,o6,o7,o8,o9,o10,o11,o12,o13,o14,o15,o16) \
o4,o3,o2,o1,\
o8,o7,o6,o5,\
o12,o11,o10,o9,\
o16,o15,o14,o13
#define out(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) \
	({p(0,0)=ri ## A;p(1,0)=ri ## B;p(2,0)=ri ## C;p(3,0)=ri ## D;\
	p(0,1)=ri ## E;p(1,1)=Q?:ri ## F;p(2,1)=Q?:ri ## G;p(3,1)=ri ## H;\
	p(0,2)=ri ## I;p(1,2)=Q?:ri ## J;p(2,2)=Q?:ri ## K;p(3,2)=ri ## L;\
	p(0,3)=ri ## M;p(1,3)=ri ## N;p(2,3)=ri ## O;p(3,3)=ri ## P;})

/* pattern which is the same if rotated or flipped */
#define patout1(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,p21,p22,p23,p24,p25,\
	o1,o2,o3,o4,o5,o6,o7,o8,o9,o10,o11,o12,o13,o14,o15,o16,m) \
{normal_meanings;\
if(pat(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,p21,p22,p23,p24,p25)){\
ap(out,o1,o2,o3,o4,o5,o6,o7,o8,o9,o10,o11,o12,o13,o14,o15,o16,m);goto drawing_done;}}

/* pattern which is the same if rotated 180 degrees or flipped */
#define patout2(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,p21,p22,p23,p24,p25,\
	o1,o2,o3,o4,o5,o6,o7,o8,o9,o10,o11,o12,o13,o14,o15,o16,m) \
{normal_meanings;\
if(pat(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,p21,p22,p23,p24,p25)){\
ap(out,o1,o2,o3,o4,o5,o6,o7,o8,o9,o10,o11,o12,o13,o14,o15,o16,m);goto drawing_done;}}\
{rot090_meanings;\
if(pat(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,p21,p22,p23,p24,p25)){\
	ap(out,ap(rot4x4,o1,o2,o3,o4,o5,o6,o7,o8,o9,o10,o11,o12,o13,o14,o15,o16),m);goto drawing_done;}}

/* match pattern, and rotated pattern, asume it's the same if flipped */
#define patout4(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,p21,p22,p23,p24,p25,\
	o1,o2,o3,o4,o5,o6,o7,o8,o9,o10,o11,o12,o13,o14,o15,o16,m) \
{normal_meanings;\
if(pat(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,p21,p22,p23,p24,p25)){\
	ap(out,o1,o2,o3,o4,o5,o6,o7,o8,o9,o10,o11,o12,o13,o14,o15,o16,m);goto drawing_done;}}\
{rot090_meanings;\
if(pat(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,p21,p22,p23,p24,p25)){\
	ap(out,ap(rot4x4,o1,o2,o3,o4,o5,o6,o7,o8,o9,o10,o11,o12,o13,o14,o15,o16),m);goto drawing_done;}}\
{rot180_meanings;\
if(pat(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,p21,p22,p23,p24,p25)){\
	ap(out,ap(rot4x4,ap(rot4x4,o1,o2,o3,o4,o5,o6,o7,o8,o9,o10,o11,o12,o13,o14,o15,o16)),m);goto drawing_done;}}\
{rot270_meanings;\
if(pat(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,p21,p22,p23,p24,p25)){\
	ap(out,ap(rot4x4,ap(rot4x4,ap(rot4x4,o1,o2,o3,o4,o5,o6,o7,o8,o9,o10,o11,o12,o13,o14,o15,o16))),m);goto drawing_done;}}

/* match pattern, and rotated pattern, and flipped, rotated pattern */
#define patout8(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,p21,p22,p23,p24,p25,\
	o1,o2,o3,o4,o5,o6,o7,o8,o9,o10,o11,o12,o13,o14,o15,o16,m) \
{normal_meanings;\
if(pat(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,p21,p22,p23,p24,p25)){\
	ap(out,o1,o2,o3,o4,o5,o6,o7,o8,o9,o10,o11,o12,o13,o14,o15,o16,m);goto drawing_done;}}\
{rot090_meanings;\
if(pat(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,p21,p22,p23,p24,p25)){\
	ap(out,ap(rot4x4,o1,o2,o3,o4,o5,o6,o7,o8,o9,o10,o11,o12,o13,o14,o15,o16),m);goto drawing_done;}}\
{rot180_meanings;\
if(pat(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,p21,p22,p23,p24,p25)){\
	ap(out,ap(rot4x4,ap(rot4x4,o1,o2,o3,o4,o5,o6,o7,o8,o9,o10,o11,o12,o13,o14,o15,o16)),m);goto drawing_done;}}\
{rot270_meanings;\
if(pat(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,p21,p22,p23,p24,p25)){\
	ap(out,ap(rot4x4,ap(rot4x4,ap(rot4x4,o1,o2,o3,o4,o5,o6,o7,o8,o9,o10,o11,o12,o13,o14,o15,o16))),m);goto drawing_done;}}\
{flp180_meanings;\
if(pat(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,p21,p22,p23,p24,p25)){\
	ap(out,ap(flp4x4,o1,o2,o3,o4,o5,o6,o7,o8,o9,o10,o11,o12,o13,o14,o15,o16),m);goto drawing_done;}}\
{flp270_meanings;\
if(pat(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,p21,p22,p23,p24,p25)){\
	ap(out,ap(rot4x4,ap(flp4x4,o1,o2,o3,o4,o5,o6,o7,o8,o9,o10,o11,o12,o13,o14,o15,o16)),m);goto drawing_done;}}\
{fliped_meanings;\
if(pat(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,p21,p22,p23,p24,p25)){\
	ap(out,ap(rot4x4,ap(rot4x4,ap(flp4x4,o1,o2,o3,o4,o5,o6,o7,o8,o9,o10,o11,o12,o13,o14,o15,o16))),m);goto drawing_done;}}\
{flp090_meanings;\
if(pat(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,p21,p22,p23,p24,p25)){\
	ap(out,ap(rot4x4,ap(rot4x4,ap(rot4x4,ap(flp4x4,o1,o2,o3,o4,o5,o6,o7,o8,o9,o10,o11,o12,o13,o14,o15,o16)))),m);goto drawing_done;}}

#define M(x) (riM==x)
#define N(x) (riN==x)
#define G(x) (riG==x)
#define H(x) (riH==x)
#define R(x) (riR==x)
#define NG(x) (riG!=x)
#define NH(x) (riH!=x)
#define NM(x) (riM!=x)
#define Z(x) (1==1)
/*ABCDE
  FGHIJ   GHI
  KLMNO   LMN
  PQRST   QRS
  UVWXY*/
ap9(patout2,
Z,Z,Z,
M,Z,M,
Z,Z,Z,
M,M,M,M,
M,M,M,M,
M,M,M,M,
M,M,M,M,0);
ap9(patout1,
H,H,H,
H,Z,H,
H,H,H,
H,M,M,H,
M,M,M,M,
M,M,M,M,
H,M,M,H,0);
ap3(patout4,
Z,H,H,H,Z,
Z,M,NM,M,Z,
Z,NM,Z,NM,Z,
Z,M,NM,M,Z,
Z,Z,Z,Z,Z,
M,M,M,M,
M,M,M,M,
M,M,M,M,
M,M,M,M,0);
ap3(patout4,
M,M,H,H,Z,
M,M,H,H,Z,
H,H,Z,M,Z,
H,H,M,M,Z,
Z,Z,Z,Z,Z,
M,M,M,M,
M,M,M,M,
M,M,M,M,
M,M,M,M,0);
ap9(patout4,
H,H,M,
H,Z,N,
M,N,N,
H,M,M,M,
M,M,M,M,
M,M,M,M,
M,M,M,N,0);
ap9(patout8,
H,H,H,
H,Z,M,
M,R,R,
H,H,M,M,
M,M,M,M,
M,M,M,M,
M,M,M,M,0);
ap9(patout4,
H,H,H,
H,Z,H,
H,H,Z,
H,M,M,H,
M,M,M,M,
M,M,M,M,
H,M,M,M,0);
ap9(patout1,
Z,H,Z,
H,Z,H,
Z,H,Z,
H,M,M,H,
M,M,M,M,
M,M,M,M,
H,M,M,H,0);
ap9(patout8,
Z,H,Z,
H,Z,H,
Z,Z,Z,
H,M,M,H,
M,M,M,M,
M,M,M,M,
M,M,M,M,0);
ap9(patout4,
Z,NM,H,
H,Z ,Z,
H,Z ,Z,
H,H,M,M,
H,M,M,M,
M,M,M,M,
M,M,M,M,0);
ap9(patout8,
Z,NM,H,
H,Z ,M,
Z,Z ,Z,
H,H,H,M,
H,M,M,M,
M,M,M,M,
M,M,M,M,0);
ap9(patout4,
Z,NM,Z,
H,Z ,R,
Z,NM,Z,
H,H,M,M,
H,M,M,M,
M,M,M,R,
M,M,R,R,0);
ap9(patout4,
Z,NM,Z,
H,Z ,Z,
Z,Z ,Z,
H,H,M,M,
H,M,M,M,
M,M,M,M,
M,M,M,M,0);
/*default if no patterns match*/
{normal_meanings;
ap(out,
M,M,M,M,
M,M,M,M,
M,M,M,M,
M,M,M,M,0);}
drawing_done:;
}}/*end fors*/

#ifdef MUHUHAHAHA
#define outshft(X,Y) \
      ({o(0,0)=p(X,Y  );o(1,0)=p(X+1,Y  );o(2,0)=p(X+2,Y  );o(3,0)=p(X+3,Y  );\
        o(0,1)=p(X,Y+1);o(1,1)=p(X+1,Y+1);o(2,1)=p(X+2,Y+1);o(3,1)=p(X+3,Y+1);\
        o(0,2)=p(X,Y+2);o(1,2)=p(X+1,Y+2);o(2,2)=p(X+2,Y+2);o(3,2)=p(X+3,Y+2);\
        o(0,3)=p(X,Y+3);o(1,3)=p(X+1,Y+3);o(2,3)=p(X+2,Y+3);o(3,3)=p(X+3,Y+3);})
for(j=0;j<Yres;j++){
for(i=0;i<Xres;i++){
/*now, we check if the current frame locally looks like a previous frame, within the last few frames. */
unsigned f1,f0 = 1;//how many frames back, the first frame differing is.
unsigned direction;
int id=0,jd=0,curscrl;
find_scroll_direction(direction,f0,f1,Xres,Yres,i,j);
if(!direction)goto fail;// this pixel is staying the same.
if(direction&(direction-1)){//more than one bit set, it is ambiguous, so check if pixels nearby are unambiguous
	unsigned d0,d1,nf0,nf1;
	/* if a nearby pixel has a valid direction, and it is one of the 
	   directions this pixel could go, go in that direction. */
#define disambiguate(I,J) \
	find_scroll_direction(d1,f0,f1,Xres,Yres,i+I,j+J);\
	d0 = direction & d1;\
	if(d0 && !(d0&(d0-1))){\
		direction=d0;\
		goto success;\
	}
	disambiguate(1,0);
	disambiguate(-1,0);
	disambiguate(0,1);
	disambiguate(0,-1);
	disambiguate(1,1);
	disambiguate(1,-1);
	disambiguate(-1,-1);
	disambiguate(-1,1);
	disambiguate(2,0);
	disambiguate(-2,0);
	disambiguate(0,2);
	disambiguate(0,-2);
	goto fail;
}
success:;
if(direction == dir_right)id=1,jd=0;
if(direction == dir_left)id=-1,jd=0;
if(direction == dir_up)id=0,jd=-1;
if(direction == dir_down)id=0,jd=1;
/*now we have cur, f0, f1. consider the graph of rounded-scroll versus interpolated-scroll as:
Interpolated
-3 -2 -1  0  1  2  3  4
                 /-----   1
    /-----------/         0
---/                     -1 Rounded
And consider that the changes happen at scroll +2 or -2.
Hence, at f1 the scroll in out pixels should be -6 and at f0 it should be -2:
so at any f, the scroll should be (f0-f)*4/(f1-f0)-2. for f=0, the current frame,
this works out to f0*4/(f1-f0)-2 */
curscrl = f0*4/(f1-f0)-2;
/*things shouldn't scroll more than 4 pixels, because by the fourth pixel, 
if scroll speed is constant, the input should have changed.*/
if(curscrl>4)curscrl=0;
if(curscrl<-4)curscrl=0;
outshft(id*curscrl,jd*curscrl);
continue;
fail:;//fail, do nothing, the result is as if there was no interpolation.
outshft(0,0);

}}/*end fors*/
thisframen++;
thisframen%=N_PREVFRAMES;
#endif
}


static void find_scroll_direction(unsigned &direction,unsigned &f0,unsigned &f1,int Xres,int Yres,int i,int j){
direction = 0;/* if things don't go well, we'll have a direction of zero, indicating we couldn't find a good way to scroll. */
f0=1;
if(i<0||i>=Xres||j<0||j>=Yres)return;
unsigned curhash = prevframe[thisframen][i+j*Xres];
unsigned fn0;
/* first, we find out if this pixel has changed */
while(1){
	if(f0>(N_PREVFRAMES-1)/2)return;/* the scroll is either too slow, or there isn't one. */
	fn0 = (thisframen-f0+N_PREVFRAMES)%N_PREVFRAMES;
	if(!prevframe[fn0])return;
	if(prevframe[fn0][i+j*Xres]!=curhash)break;
	f0++;
}
/* now we find the scroll direction, and then find a frame which
   is scrolled even further in the past, if such scroll would go offscreen,
   dismiss that direction as invalid ( but still try others ). */
if(i+2<Xres&&prevframe[fn0][i+1+j*Xres]==curhash){
	f1=f0+1;
	while(1){
		if(f1>N_PREVFRAMES-1)break;
		unsigned fn1 = (thisframen-f1+N_PREVFRAMES)%N_PREVFRAMES;
		if(!prevframe[fn1])break;
		if(prevframe[fn1][i+2+j*Xres]==curhash){direction |= dir_right;break;}
		f1++;
	}
}
if(i-2>=0&&prevframe[fn0][i-1+j*Xres]==curhash){
	f1=f0+1;
	while(1){
		if(f1>N_PREVFRAMES-1)break;
		unsigned fn1 = (thisframen-f1+N_PREVFRAMES)%N_PREVFRAMES;
		if(!prevframe[fn1])break;
		if(prevframe[fn1][i-2+j*Xres]==curhash){direction |= dir_left;break;}
		f1++;
	}
}
if(j+2<Yres&&prevframe[fn0][i+(j+1)*Xres]==curhash){
	f1=f0+1;
	while(1){
		if(f1>N_PREVFRAMES-1)break;
		unsigned fn1 = (thisframen-f1+N_PREVFRAMES)%N_PREVFRAMES;
		if(!prevframe[fn1])break;
		if(prevframe[fn1][i+(j+2)*Xres]==curhash){direction |= dir_down;break;}
		f1++;
	}
}
if(j-2>=0&&prevframe[fn0][i+(j-1)*Xres]==curhash){
	f1=f0+1;
	while(1){
		if(f1>N_PREVFRAMES-1)break;
		unsigned fn1 = (thisframen-f1+N_PREVFRAMES)%N_PREVFRAMES;
		if(!prevframe[fn1])break;
		if(prevframe[fn1][i+(j-2)*Xres]==curhash){direction |= dir_up;break;}
		f1++;
	}
}
return;
}
