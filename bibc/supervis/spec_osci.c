/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF spec_osci supervis  DATE 22/03/2011   AUTEUR COURTOIS M.COURTOIS */
/* ================================================================== */
/* COPYRIGHT (C) 1991 - 2011  EDF R&D              WWW.CODE-ASTER.ORG */
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
#include <math.h>
#include <stdio.h>
#include "aster.h"

/*
#define __DEBUG__
*/
double STDCALL(R8PI, r8pi)();

void calc_SPEC_OSCI( int nbpts, double* vale_x, double* vale_y,
                     int len_f, double* l_freq, int len_a, double* l_amor,
                     double* spectr )
{
   int      i,a,f;
   double   pas, DELTAT, eps, ecart;
   double   XSI, XSI2, W, W2, UNSW2, WDT, COSWDT, SINWDT, A, B, C, D, E, F;
   double   EXPXWT, EXPSA, D1, D2, D3, V2, V3, F1, F2, FSDT, DEUXPI;
   
   eps = 1.e-6;

#ifdef __DEBUG__
   printf("<I> <SPEC_OSCI> INTEGRATION PAR LA METHODE DE NIGAM_JENNINGS\n");
#endif

   /* pas constant ou variable */
   DELTAT = vale_x[1]-vale_x[0];
   ecart = 0.;
   i = 1;
   while ( i<nbpts-1 && ecart<eps ) {
      pas = vale_x[i+1]-vale_x[i];
      if ( fabs((pas-DELTAT)/DELTAT) > ecart )
         ecart = fabs((pas-DELTAT)/DELTAT);
      i++;
   }

   DEUXPI = (double)(2. * F_FUNC(R8PI, r8pi)());
#ifdef __DEBUG__
   printf("2*pi : %lf\n", DEUXPI);
#endif
      
   /* le pas est constant */
   if (ecart < eps)
   {
#ifdef __DEBUG__
      printf("   AVEC UN PAS CONSTANT\n");
      printf("      VALEUR DU PAS     : %lf\n", DELTAT);
      printf("      ECART RELATIF MAX : %lf\n", ecart);
#endif

      for (a=0; a<len_a; a++) {
         XSI = l_amor[a];
         XSI2 = XSI * XSI;
         for (f=0; f<len_f; f++) {
            W = DEUXPI * l_freq[f];
            W2 = W * W;
            UNSW2 = 1. / W2;
            WDT = W * DELTAT;
            COSWDT = cos( WDT );
            SINWDT = sin( WDT );
            A = W * sqrt( 1. - XSI2 );
            B = 2. * XSI / W;
            C = ( ( 2. * XSI2 ) - 1. ) / A;
            D = XSI * W;
            E = W2 / A;
            F = COSWDT + ( SINWDT * D / A );
            EXPXWT = exp( -D * DELTAT );
            EXPSA = EXPXWT / A;
            D1 = 0.;
            D2 = 0.;
            V2 = 0.;
            F1 = vale_y[0];
            for (i=1; i<nbpts; i++) {
               F2 = vale_y[i];
               FSDT = ( F2 - F1 ) / DELTAT;
               D3 = EXPSA*((A*COSWDT+D*SINWDT)*D2+SINWDT*V2)+UNSW2*
                    (EXPXWT*(FSDT*(B*COSWDT+C*SINWDT)-F1*F)+F2-B*FSDT);
               V3 = EXPSA*((-W2)*SINWDT*D2+((-D)*SINWDT+A*COSWDT)*V2) +
                    UNSW2*(EXPXWT*((-FSDT)*F+F1*E*SINWDT)+FSDT);
               D2 = D3;
               V2 = V3;
               if ( fabs(D3) > D1 )
                  D1 = fabs(D3);
               F1 = F2;
            }
            /* spectr[i, j, k] = spectr[i*dim2*dim3 + j*dim3 + k] */
            spectr[a*3*len_f + 0*len_f + f] = D1;
            spectr[a*3*len_f + 1*len_f + f] = W * D1;
            spectr[a*3*len_f + 2*len_f + f] = W2 * D1;
#ifdef __DEBUG__
            printf("spectr[%d,%d,%d] = %lf\n", a, 0, f, a*len_f*3 + 0*len_f + f, D1);
            printf("spectr[%d,%d,%d] = %lf\n", a, 1, f, a*len_f*3 + 1*len_f + f, W*D1);
            printf("spectr[%d,%d,%d] = %lf\n", a, 2, f, a*len_f*3 + 2*len_f + f, W2*D1);
            printf("#C %d %d %lf %lf\n", a+1, f+1, D1, W);
#endif
         }
      }
   }
   /* le pas est variable */
   else
   {
#ifdef __DEBUG__
      printf("   AVEC UN PAS NON CONSTANT\n");
#endif

      for (a=0; a<len_a; a++) {
         XSI = l_amor[a];
         XSI2 = XSI * XSI;
         for (f=0; f<len_f; f++) {
            W = DEUXPI * l_freq[f];
            W2 = W * W;
            UNSW2 = 1. / W2;
            D1 = 0.;
            D2 = 0.;
            V2 = 0.;
            F1 = vale_y[0];

            for (i=1; i<nbpts; i++) {
               DELTAT = vale_x[i] - vale_x[i-1];
               F2 = vale_y[i];
               WDT = W * DELTAT;
               COSWDT = cos( WDT );
               SINWDT = sin( WDT );
               A = W * sqrt( 1. - XSI2 );
               B = 2. * XSI / W;
               C = ( ( 2. * XSI2 ) - 1. ) / A;
               D = XSI * W;
               E = W2 / A;
               F = COSWDT + ( SINWDT * D / A );
               EXPXWT = exp( -D * DELTAT );
               EXPSA = EXPXWT / A;
               FSDT = ( F2 - F1 ) / DELTAT;
               D3 = EXPSA*((A*COSWDT+D*SINWDT)*D2+SINWDT*V2)+UNSW2*
                    (EXPXWT*(FSDT*(B*COSWDT+C*SINWDT)-F1*F)+F2-B*FSDT);
               V3 = EXPSA*((-W2)*SINWDT*D2+((-D)*SINWDT+A*COSWDT)*V2) +
                    UNSW2*(EXPXWT*((-FSDT)*F+F1*E*SINWDT)+FSDT);
               D2 = D3;
               V2 = V3;
               if ( fabs(D3) > D1 )
                  D1 = fabs(D3);
               F1 = F2;
            }
            /* spectr[i, j, k] = spectr[i*dim2*dim3 + j*dim3 + k] */
            spectr[a*3*len_f + 0*len_f + f] = D1;
            spectr[a*3*len_f + 1*len_f + f] = W * D1;
            spectr[a*3*len_f + 2*len_f + f] = W2 * D1;
#ifdef __DEBUG__
            printf("spectr[%d,%d,%d] = %lf\n", a, 0, f, a*len_f*3 + 0*len_f + f, D1);
            printf("spectr[%d,%d,%d] = %lf\n", a, 1, f, a*len_f*3 + 1*len_f + f, W*D1);
            printf("spectr[%d,%d,%d] = %lf\n", a, 2, f, a*len_f*3 + 2*len_f + f, W2*D1);
            printf("#C %d %d %lf %lf\n", a+1, f+1, D1, W);
#endif
         }
      }
   }
}
