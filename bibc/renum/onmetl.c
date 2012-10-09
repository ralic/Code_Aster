/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF onmetl renum  DATE 08/10/2012   AUTEUR LEFEBVRE J-P.LEFEBVRE */
/* ================================================================== */
/* COPYRIGHT (C) 1991 - 2012  EDF R&D              WWW.CODE-ASTER.ORG */
/*                                                                    */
/* THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR      */
/* MODIFY IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS     */
/* PUBLISHED BY THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE */
/* LICENSE, OR (AT YOUR OPTION) ANY LATER VERSION.                    */
/* THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL,    */
/* BUT WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF     */
/* MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU   */
/* GENERAL PUBLIC LICENSE FOR MORE DETAILS.                           */
/*                                                                    */
/* YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE  */
/* ALONG WITH THIS PROGRAM; IF NOT, WRITE TO : EDF R&D CODE_ASTER,    */
/*    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.     */
/* ================================================================== */

#include "aster.h"

#ifdef _HAVE_METIS
#include "metis.h"
#endif


void DEFPPPPPPPPPPPPP(ONMETL,onmetl, nbnd,nadj,
                        xadjd, adjncy,
               		   invpnd,permnd,supnd, parent,
	      nbsn,nbops,fctnzs, lgind, 
	      niv)
     INTEGER4 *nbnd,*nadj,  *xadjd, *adjncy;
     INTEGER4 *invpnd, *permnd,*supnd, *parent,*nbsn;
     DOUBLE *nbops;
     INTEGER4 *fctnzs, *lgind;
     INTEGER *niv;
{
#ifdef _HAVE_METIS
  /*
Donnees:
--------
nbnd : Nombre de noeuds
nadj : Nombre d aretes du graphe de connexion
xadj[1:nbnd+1]: pointeur de adjncy
adjncy[1:nadj] : connexion du graphe, voisins de chaque noeud

niv : niveau des impressions 0, 1, 2 

Resultats:
---------
invpnd[1:nbnd] : Renumerotation des noeuds (nouveau numero)
permnd[1:nbnd] : inverse du precedent (ancien numero)
nbsn : Nombre de super noeuds
parent[1:nbsn] : arborescence des super noeuds
supnd[1:nbnd+1] : definition des supernoeuds

nbops : Nombre d operation flottantes pour la factorisation a effectuer
fctnzs : Nombre de termes non nuls dans la matrice factorisee
lgind  : longueur de tableaux de pointeurs
   */
  int n,m,i;
  n=*nbnd;
  m=*nadj;

  int  options[10],ret;
   idxtype *perm, *iperm; 
   GraphType graph; 
   char filename[256]; 
   int numflag = 0, wgtflag; 
   timer TOTALTmr, METISTmr, IOTmr, SMBTmr; 
   strcpy(filename, "fort.81"); 
   
   cleartimer(TOTALTmr); 
   cleartimer(METISTmr); 
   cleartimer(IOTmr); 
   cleartimer(SMBTmr); 

   starttimer(TOTALTmr); 
   starttimer(IOTmr);
   ret = ReadGraphL(&graph,nbnd,nadj,xadjd,adjncy , &wgtflag);
   if (graph.nvtxs <= 0) { 
     printf("Empty graph. Nothing to do.\n"); 
     exit(4); 
   } 
   if (graph.ncon != 1) { 
     printf("Ordering can only be applied to graphs with one constraint.\n"); 
     exit(4); 
   } 
   stoptimer(IOTmr); 

   /* Ordering does not use weights! */ 
   GKfree(&graph.vwgt, &graph.adjwgt, LTERM); 

   if (*niv > 1 ) {
     printf("**********************************************************************\n"); 
     printf("%s", METISTITLE); 
     printf("Graph Information ---------------------------------------------------\n"); 
     printf("  Name: %s, #Vertices: %d, #Edges: %d\n\n", filename, graph.nvtxs, graph.nedges/2); 
     printf("Node-Based Ordering... ----------------------------------------------\n"); 
   }
   perm = idxmalloc(graph.nvtxs, "main: perm"); 
   iperm = idxmalloc(graph.nvtxs, "main: iperm"); 
   options[0] = 0; 
 
   starttimer(METISTmr); 
   METIS_NodeND(&graph.nvtxs, graph.xadj, graph.adjncy, &numflag, options, perm, iperm); 
   stoptimer(METISTmr); 

   starttimer(IOTmr); 
   WritePermutation(filename, iperm, graph.nvtxs);

   stoptimer(IOTmr); 
   starttimer(SMBTmr); 
   ret = ComputeFillInL(&graph, iperm,parent,
		  supnd,&graph.nvtxs,nbsn,nbops,lgind,fctnzs);
   stoptimer(SMBTmr); 

   stoptimer(TOTALTmr); 
   
   if (*niv > 1 ) {
     printf("\nTiming Information --------------------------------------------------\n"); 
     printf("  I/O:                     \t %7.3f\n", gettimer(IOTmr)); 
     printf("  Ordering:                \t %7.3f   (ONMETIS time)\n", gettimer(METISTmr)); 
     printf("  Symbolic Factorization:  \t %7.3f\n", gettimer(SMBTmr)); 
     printf("  Total:                   \t %7.3f\n", gettimer(TOTALTmr)); 
     printf("**********************************************************************\n"); 
   }


   
   {int kkk;
 
   for(kkk=0;kkk<graph.nvtxs;kkk++)
     {invpnd[kkk]= iperm[kkk]; 
       permnd[ invpnd[kkk] ]= kkk;
 
     } 
    for(kkk=0;kkk<graph.nvtxs;kkk++)
      { invpnd[kkk]+=1;
	permnd[kkk]+=1;
      }
   }
    GKfree(&graph.xadj, &graph.adjncy, &perm, &iperm, LTERM); 
     
#else    
  
    CALL_U2MESS("F", "FERMETUR_15");
  
           
#endif
}


#ifdef _HAVE_METIS
 /************************************************************************* 
 * This function reads the spd matrix 
 **************************************************************************/ 

int ReadGraphL(GraphType *graph, int *nbnd,int *nadj,int *xadjd,int *adjnci, int *wgtflag)
{
  int i, j, k, l, fmt, readew, readvw, ncon, edge, ewgt;
  idxtype *xadj, *adjncy, *vwgt, *adjwgt;
  char *line, *oldstr, *newstr;
  FILE *fpin;

  InitGraph(graph);

  line = (char *)malloc(sizeof(char)*(MAXLINE+1));
  /*
  if ((fpin = fopen(filename, "r")) == NULL) {
    printf("Failed to open file %s\n", filename);
    exit(0);
  }
  
  do {
    fgets(line, MAXLINE, fpin);
  } while (line[0] == '%' && !feof(fpin));

  if (feof(fpin)) {
    graph->nvtxs = 0;
    free(line);
    return;
  }
  */
  graph->nvtxs = *nbnd;
  fmt = ncon = 0;
  /* sscanf(line, "%d %d %d %d", &(graph->nvtxs), &(graph->nedges), &fmt, &ncon);
   */
  graph->nedges=*nadj;
  readew = (fmt%10 > 0);
  readvw = ((fmt/10)%10 > 0);
  if (fmt >= 100) {
    printf("Cannot read this type of file format!");
    exit(0);
  }


  *wgtflag = 0;
  if (readew)
    *wgtflag += 1;
  if (readvw)
    *wgtflag += 2;

  if (ncon > 0 && !readvw) {
    printf("------------------------------------------------------------------------------\n");
    printf("***  I detected an error in your input file  ***\n\n");
    printf("You specified ncon=%d, but the fmt parameter does not specify vertex weights\n", ncon);
    printf("Make sure that the fmt parameter is set to either 10 or 11.\n");
    printf("------------------------------------------------------------------------------\n");
    exit(0);
  }

  /*  graph->nedges *=2;mis directement a nadj */
  ncon = graph->ncon = (ncon == 0 ? 1 : ncon);

  /*printf("%d %d %d %d %d [%d %d]\n", fmt, fmt%10, (fmt/10)%10, ncon, graph->ncon, readew, readvw);*/

  if (graph->nvtxs > MAXIDX) 
    errexit("\nThe matrix is too big: %d [%d %d]\n", graph->nvtxs, MAXIDX, sizeof(idxtype));

  xadj = graph->xadj = idxsmalloc(graph->nvtxs+1, 0, "ReadGraph: xadj");
  adjncy = graph->adjncy = idxmalloc(graph->nedges, "ReadGraph: adjncy");

  vwgt = graph->vwgt = (readvw ? idxmalloc(ncon*graph->nvtxs, "ReadGraph: vwgt") : NULL);
  adjwgt = graph->adjwgt = (readew ? idxmalloc(graph->nedges, "ReadGraph: adjwgt") : NULL);
 

  for(k=0;k<=graph->nvtxs;k++)
    {
      xadj[k] = xadjd[k]-1; /* -1 on est en C */
    }
  int ll=graph->nedges;
  for(k=0;k<ll;k++) 
    { 
         adjncy[k]=adjnci[k]-1; /*  on est en C */
    }
 
  free(line);
  return 0;
}


 /************************************************************************* 
 * This function writes out the partition vector 
 **************************************************************************/ 
 void WritePermutation(char *fname, idxtype *iperm, int n) 
 { 
   FILE *fpout; 
   int i; 
   char filename[256]; 

   sprintf(filename,"%s.iperm",fname); 

   if ((fpout = fopen(filename, "w")) == NULL)  
     errexit("Problems in opening the permutation file: %s", filename); 

   for (i=0; i<n; i++) 
     fprintf(fpout,"%d\n", iperm[i]); 

   fclose(fpout); 

 } 





 

/************************************************************************* 
 * This function sets up data structures for fill-in computations 
 **************************************************************************/ 
int ComputeFillInL(GraphType *graph, idxtype *iperm,idxtype *parent,
		    idxtype *supnd,int *neq,int *nbsn,double *opc,int *lgindd,int *maxlnz)
 { 
   int i, j, k, nvtxs, maxsub,lgind;
   idxtype *xadj, *adjncy; 
   idxtype *perm, *xlnz, *xnzsub, *nzsub; 


 
/*   printf("\nSymbolic factorization... --------------------------------------------\n"); */
 

   nvtxs = graph->nvtxs; 
   xadj = graph->xadj; 
   adjncy = graph->adjncy; 

   maxsub = 4*xadj[nvtxs]; 

   /* Relabel the vertices so that it starts from 1 */ 
   k = xadj[nvtxs]; 
   for (i=0; i<k; i++)
     {
       adjncy[i]++;/*  printf(" adjncy %d \n",adjncy[i]);*/
     }
   for (i=0; i<nvtxs+1; i++) 
     { xadj[i]++; /*  printf(" xadj %d \n",xadj[i]);*/
     }

   /* Allocate the required memory */ 
   perm = idxmalloc(nvtxs+1, "ComputeFillIn: perm"); 
   xlnz = idxmalloc(nvtxs+1, "ComputeFillIn: xlnz"); 
   xnzsub = idxmalloc(nvtxs+1, "ComputeFillIn: xnzsub"); 
   nzsub = idxmalloc(maxsub, "ComputeFillIn: nzsub"); 

   /* Construct perm from iperm and change the numbering of iperm */ 
   for (i=0; i<nvtxs; i++) 
 { 
     perm[iperm[i]] = i; 
 } 
   for (i=0; i<nvtxs; i++) { 
     iperm[i]++; 
     perm[i]++; 
   } 
    
   /* 
    * Call sparspak routine. 
    */ 
   int errs;
   errs=smbfctl(nvtxs, xadj, adjncy, perm, iperm, xlnz, maxlnz, xnzsub, nzsub, &maxsub,supnd,parent,nbsn,&lgind,opc);
/*   printf("errs %d \n",errs);*/
   if (errs) {
     int errs2;
     free(nzsub); 

     maxsub = 4*maxsub;  
     nzsub = idxmalloc(maxsub, "ComputeFillIn: nzsub");
     errs2= smbfctl(nvtxs, xadj, adjncy, perm, iperm, xlnz, maxlnz, xnzsub, nzsub, &maxsub,supnd,parent,nbsn,&lgind,opc);
/*   printf("errs2 %d \n",errs2);*/
     if (errs2)  
       errexit("MAXSUB is too small!"); 
   } 

   
   
  
   GKfree(&perm, &xlnz, &xnzsub, &nzsub, LTERM); 


   /* Relabel the vertices so that it starts from 0 */ 
   for (i=0; i<nvtxs; i++) 
     iperm[i]--; 
   for (i=0; i<nvtxs+1; i++) 
     xadj[i]--; 
   k = xadj[nvtxs]; 
   for (i=0; i<k; i++)
     {
       adjncy[i]--;
     }
   *lgindd=lgind;
   return 0;
 }




 /*****************************************************************           
 **********     SMBFCT ..... SYMBOLIC FACTORIZATION       *********  
 ******************************************************************           
 *   PURPOSE - THIS ROUTINE PERFORMS SYMBOLIC FACTORIZATION                
 *   ON A PERMUTED LINEAR SYSTEM AND IT ALSO SETS UP THE                
 *   COMPRESSED DATA STRUCTURE FOR THE SYSTEM.                          
 * 
 *   INPUT PARAMETERS -                                                
 *      NEQNS - NUMBER OF EQUATIONS.                                  
 *      (XADJ, ADJNCY) - THE ADJACENCY STRUCTURE.                    
 *      (PERM, INVP) - THE PERMUTATION VECTOR AND ITS INVERSE.      
 * 
 *   UPDATED PARAMETERS -                                          
 *      MAXSUB - SIZE OF THE SUBSCRIPT ARRAY NZSUB.  ON RETURN,   
 *             IT CONTAINS THE NUMBER OF SUBSCRIPTS USED         
 * 
 *   OUTPUT PARAMETERS -                                        
 *      XLNZ - INDEX INTO THE NONZERO STORAGE VECTOR LNZ.    
 *      (XNZSUB, NZSUB) - THE COMPRESSED SUBSCRIPT VECTORS.  
 *      MAXLNZ - THE NUMBER OF NONZEROS FOUND.              
 * 
 *******************************************************************/ 
 int smbfctl(int neqns, idxtype *xadj, idxtype *adjncy, idxtype *perm, idxtype *invp, idxtype *xlnz,  
	     int *maxlnz, idxtype *xnzsub, idxtype *nzsub, int *maxsub,idxtype *supnd,idxtype *parsnode,int *nbsn,int *lgin,double *opc)
 { 
   /* Local variables */ 
   int snode,node, rchm, mrgk, lmax, i, j, k, m, nabor, nzbeg, nzend;
   int kxsub, jstop, jstrt, mrkflg, inz, knz, flag,lgind; 
   idxtype *mrglnk, *marker, *rchlnk; 
 /* jyb */ 
   int  lnode, xnzi, xnzip1,nbi, nbip1; 
   int aux, minsn, maxsn; 

   int taille1, k1, taille2, k2, taille3, k3,nd; 
   /* idxtype *parent, *nliens, *supnd, *tbnode, *parsnode;*/
   idxtype *parent, *nliens, *tbnode; 
   FILE *fpout; 
   /*  extern void ecri11_; */ 
 /* jyb fin */ 

   rchlnk = idxmalloc(neqns+1, "smbfct: rchlnk"); 
   marker = idxsmalloc(neqns+1, 0, "smbfct: marker"); 
   mrglnk = idxsmalloc(neqns+1, 0, "smbfct: mgrlnk"); 

   /* Parameter adjustments */ 
   --marker; 
   --mrglnk; 
   --rchlnk; 
   --nzsub; 
   --xnzsub; 
   --xlnz; 
   --invp; 
   --perm; 
   --adjncy; 
   --xadj; 
   --parsnode;
   --supnd;

   /* Function Body */ 
   flag = 0; 
   nzbeg = 1; 
   nzend = 0; 
   xlnz[1] = 1; 

   /* FOR EACH COLUMN KNZ COUNTS THE NUMBER OF NONZEROS IN COLUMN K ACCUMULATED IN RCHLNK. */ 
   for (k = 1; k <= neqns; ++k) { 
     knz = 0; 
     mrgk = mrglnk[k]; 
     mrkflg = 0; 
     marker[k] = k; 
     if (mrgk != 0)  
       marker[k] = marker[mrgk]; 
     xnzsub[k] = nzend; 
     node = perm[k]; 

     if (xadj[node] >= xadj[node+1]) { 
       xlnz[k+1] = xlnz[k]; 
       continue; 
     } 

     /* USE RCHLNK TO LINK THROUGH THE STRUCTURE OF A(*,K) BELOW DIAGONAL */ 
     rchlnk[k] = neqns+1; 
     for (j=xadj[node]; j<xadj[node+1]; j++) { 
       nabor = invp[adjncy[j]]; 
       if (nabor <= k)  
         continue; 
       rchm = k; 

       do { 
         m = rchm; 
         rchm = rchlnk[m]; 
       } while (rchm <= nabor);  

       knz++; 
       rchlnk[m] = nabor; 
       rchlnk[nabor] = rchm; 
       if (marker[nabor] != marker[k])  
         mrkflg = 1; 
     } 

     /* TEST FOR MASS SYMBOLIC ELIMINATION */ 
     lmax = 0; 
     if (mrkflg != 0 || mrgk == 0 || mrglnk[mrgk] != 0)  
       goto L350; 
     xnzsub[k] = xnzsub[mrgk] + 1; 
     knz = xlnz[mrgk + 1] - (xlnz[mrgk] + 1); 
     goto L1400; 


     /* LINK THROUGH EACH COLUMN I THAT AFFECTS L(*,K) */ 
 L350: 
     i = k; 
     while ((i = mrglnk[i]) != 0) { 
       inz = xlnz[i+1] - (xlnz[i]+1); 
       jstrt = xnzsub[i] + 1; 
       jstop = xnzsub[i] + inz; 

       if (inz > lmax) {  
         lmax = inz; 
         xnzsub[k] = jstrt; 
       } 

       /* MERGE STRUCTURE OF L(*,I) IN NZSUB INTO RCHLNK. */  
       rchm = k; 
       for (j = jstrt; j <= jstop; ++j) { 
         nabor = nzsub[j]; 
         do { 
           m = rchm; 
           rchm = rchlnk[m]; 
         } while (rchm < nabor); 

         if (rchm != nabor) { 
           knz++; 
           rchlnk[m] = nabor; 
           rchlnk[nabor] = rchm; 
           rchm = nabor; 
         } 
       } 
     } 

     /* CHECK IF SUBSCRIPTS DUPLICATE THOSE OF ANOTHER COLUMN */ 
     if (knz == lmax)  
       goto L1400; 

     /* OR IF TAIL OF K-1ST COLUMN MATCHES HEAD OF KTH */ 
     if (nzbeg > nzend)  
       goto L1200; 

     i = rchlnk[k]; 
     for (jstrt = nzbeg; jstrt <= nzend; ++jstrt) { 
       if (nzsub[jstrt] < i)  
         continue; 

       if (nzsub[jstrt] == i)  
         goto L1000; 
       else  
         goto L1200; 
     } 
     goto L1200; 

 L1000: 
     xnzsub[k] = jstrt; 
     for (j = jstrt; j <= nzend; ++j) { 
       if (nzsub[j] != i)  
         goto L1200; 
        
       i = rchlnk[i]; 
       if (i > neqns)  
         goto L1400; 
     } 
     nzend = jstrt - 1; 

     /* COPY THE STRUCTURE OF L(*,K) FROM RCHLNK TO THE DATA STRUCTURE (XNZSUB, NZSUB) */ 
 L1200: 
     nzbeg = nzend + 1; 
     nzend += knz; 

     if (nzend > *maxsub) { 
       flag = 1; /* Out of memory */ 
       break; 
     } 

     i = k; 
     for (j=nzbeg; j<=nzend; ++j) { 
       i = rchlnk[i]; 
nzsub[j] = i;
       marker[i] = k; 
     } 
     xnzsub[k] = nzbeg; 
     marker[k] = k; 

     /* 
      * UPDATE THE VECTOR MRGLNK.  NOTE COLUMN L(*,K) JUST FOUND    
      * IS REQUIRED TO DETERMINE COLUMN L(*,J), WHERE               
      * L(J,K) IS THE FIRST NONZERO IN L(*,K) BELOW DIAGONAL.       
      */ 
 L1400: 
     if (knz > 1) {  
       kxsub = xnzsub[k]; 
       i = nzsub[kxsub]; 
       mrglnk[k] = mrglnk[i]; 
       mrglnk[i] = k; 
     } 

     xlnz[k + 1] = xlnz[k] + knz; 
   } 

   if (flag == 0) { 
     *maxlnz = xlnz[neqns] - 1; 
     *maxsub = xnzsub[neqns]; 
     xnzsub[neqns + 1] = xnzsub[neqns]; 
   } 


 parent = idxsmalloc(neqns, 0, "smbfct: parent"); 
 parent--; 
   /* Calculate the elimination tree */ 
   for (i=1; i<=neqns; i++) { 
     if (xlnz[i] < xlnz[i+1]) 
       parent[i] = nzsub[xnzsub[i]]; 
     else 
       parent[i] = -1;   /* Indicates no parent */ 
   } 
   /*  supnd = idxsmalloc(neqns+1, 0, "smbfct: supnd");
       supnd--; */
 /* super-noeuds */ 
  nbip1 = xlnz[2] -xlnz[1] ; 
  xnzip1 = xnzsub[1]; 
  i=1 ; 
  snode = 1 ; supnd[snode] = 1 ; 
  lgind = nbip1; 
  marker[i]= snode; 
  nbi = nbip1; 
  xnzi = xnzip1; 
 marker[i]= snode; 
  i = i+ 1; 
/* C.Rose ligne suivante : correction de bug 13/03/02  neqns+1 remplace neqns
dans l'instruction suivante
Il y avait un pb pour ssll102a avec le dernier SN qui ne comprend qu'un noeud*/
  while ( i < neqns+1) 
      { 
 
      nbip1 = xlnz[i+1] -xlnz[i] ; 
  xnzip1 = xnzsub[i]; 
   
  if  ( nbip1 != (nbi-1)) 
      { 
          snode ++;supnd[snode] = i;   
          lgind += nbip1; 
      } 
  else if ( xnzip1 != (xnzi+1)) 
          { 
              snode ++;supnd[snode] = i; 
              lgind += nbip1; 
          } 
 else
{/* verification que dans le  sn courant (snode) le noeud i est bien voisin du nd i-1
sinon on crée un nouveau SN 
correction des fiches 12345 et 12503  */
   if( i != nzsub[xnzsub[i-1]] )
    { snode ++;supnd[snode] = i; 
              lgind += nbip1; 
    }
 }
  nbi = nbip1; 
  xnzi = xnzip1; 
 marker[i]= snode; 
  i = i+ 1; 
     } 

  supnd[snode+1] = neqns +1; 
  lnode =neqns; 
 
  marker[neqns] = snode ;
 /* C.Rose ajout de la ligne precedante : correction de bug 11/03/02 */
  /* printf("  nbsn : %d \n",snode);
  for(i=1;i<=neqns;i++) printf("  smarker %8d %8d\n ",i,marker[i]);
 for(i=1;i<=snode+1;i++) printf("  snode %8d %8d\n ",i,supnd[i]);
  */
 /* fin de cr*/ 
 minsn = neqns; 
 maxsn = 0; 
 k1 = 0; 
 k2 = 0; 
 k3 = 0; 
 taille1 = 50; 
 taille2 = 20; 
 taille3 = 05; 
 for (i = 1; i<=snode; i++) 
 { 
   aux = supnd[i+1]-supnd[i]; 
   if(aux < minsn) minsn = aux; 
   if(aux > maxsn) maxsn = aux; 
   if(aux < taille1) k1++; 
   if(aux < taille2) k2++; 
   if(aux < taille3) k3++; 
  } 
/*  
 printf("\n "); 
 printf(" taille du plus petit super-noeud %8d\n ",minsn); 
 printf(" taille du plus grand super-noeud %8d\n ",maxsn); 
 printf(" taille moyenne d un  super-noeud %8d\n ",lnode/snode); 
 printf(" nb de super-noeuds de taille < %4d : %8d\n ",taille1,k1); 
 printf(" nb de super-noeuds de taille < %4d : %8d\n ",taille2,k2); 
 printf(" nb de super-noeuds de taille < %4d : %8d\n ",taille3,k3); 
*/
 /* for (i=1; i<=snode+1; i++) {printf(" i pt. %4d%4d\n ",i,supnd[i]);} */ 

 /* parent des super-noeuds */ 
 /* principe : on a stocke dans marker pour chaque noeud son super-noeud */ 
 /* maintenant pour chaque super-noeud on prend son dernier noeud, */ 
 /* on cherche le parent de ce noeud, puis le supernoeud auquel */ 
 /* appartient ce parent et on l'affecte comme parent au supernoeud */ 
 /* quand il n'y a pas de parent on met 0 (convention C. Rose) */ 
 /*parsnode = idxsmalloc(snode+1, 0, "smbfct: parsnode"); */
 /*parsnode--;*/
 tbnode = idxsmalloc(neqns, 0, "smbfct: tbnode"); 
 tbnode--; 
 /*sauvegarde de parent nodal */ 
 for(i = 1; i<=neqns; i++) { 
     tbnode[i] = parent[i]; 
     /*      printf(" i %4d parent %8d \n", i, tbnode[i]);*/
 } 

 for(i = 1; i<=snode; i++) 
 { 
      nd = supnd[i+1] - 1; 
     if(tbnode[nd] != -1) 
         {           
             parsnode[i] = marker[tbnode[nd]]; 
         } 
     else 
         { 
         parsnode[i] = 0; 
         } 
     /*  printf(" noeud %4d parent %8d \n ",i, parsnode[i]); */ 

 } 
  *opc = 0; 
   for (i=1; i<=neqns; i++) 
        xlnz[i]--;  
          for (i=1; i<=neqns; i++) { 
     *opc += (xlnz[i+1]-xlnz[i])*(xlnz[i+1]-xlnz[i]) - (xlnz[i+1]-xlnz[i]); 
          } 
/*  printf("    Nonzeros: %d, \tOperation Count: %6.4le \n", *maxlnz, *opc); */
  /* ecriture sur le fichier 85
  ecri11_(invp,perm,supnd,parsnode,&neqns,&snode,&opc,maxlnz,&lgind); 
  */
  *nbsn=snode;
  *lgin=lgind;
   marker++; 
   mrglnk++; 
   rchlnk++; 
   nzsub++; 
   xnzsub++; 
   xlnz++; 
   invp++; 
   perm++; 
   adjncy++; 
   xadj++; 
   GKfree(&rchlnk, &mrglnk, &marker, LTERM);
   parsnode++;
   supnd++;
   
    
  
   return flag; 
   
    
 }  

#endif
