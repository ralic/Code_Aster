        SUBROUTINE FGPIC2( METHOD,RTRV,POINT,NPOINT,PIC,NPIC)
C       ----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 07/01/98   AUTEUR CIBHHLB L.BOURHRARA 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR   
C (AT YOUR OPTION) ANY LATER VERSION.                                 
C
C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT 
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF          
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU    
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.                            
C
C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE   
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,       
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
C       ----------------------------------------------------------------
C       REARRANGEMENT ET EXTRACTION DES PICS
C       ----------------------------------------------------------------
C       IN  POINT  VECTEUR DES POINTS
C           NPOINT  NOMBRE  DE  POINTS
C           RTRV  VECTEUR  DE TRAVAIL REEL
C           METHOD  METHODE DE  DEXTRACTION DES PICS EMPLOYEE
C       OUT PIC  VECTEUR DES PICS
C           NPIC  NOMBRE  DE  PICS (NPIC = NPOINT   AU MAXIMUM)
C       ----------------------------------------------------------------
C
        IMPLICIT REAL*8 (A-H,O-Z)
        CHARACTER*(*)  METHOD
        CHARACTER*16  K16B
        REAL*8    POINT(*), PIC(*), RTRV(*), PMAX
        REAL*8    DP1,      DP2
        INTEGER    NPOINT,   NPIC, NMAX, NTRV
C       ----------------------------------------------------------------
C -     EXTRACTION DES PICS POUR RAINFLOW=PIC LE PLUS GRAND EN DEBUT
C       ----------------------------------------------------------------
          IF ( METHOD.EQ.'RAINFLOW' ) THEN
C
C -       RECHERCHE DU POINT LE PLUS GRAND
C
          PMAX = POINT(1)
          NMAX = 1
            DO 8 I = 2 , NPOINT
            IF(ABS(POINT(I)).GT.PMAX)THEN
            PMAX = POINT(I)
            NMAX = I
            ENDIF
  8         CONTINUE
C
C -       REARANGEMENT AVEC POINT LE PLUS GRAND AU DEBUT ET A LA FIN
C
            DO 5 I = NMAX,NPOINT
            RTRV(I-NMAX+1) = POINT(I)
 5          CONTINUE
            DO 6 I = 1,NMAX-1
            RTRV(NPOINT-NMAX+1+I) = POINT(I)
 6          CONTINUE
          NTRV = NPOINT
C
C -       EXTRACTION DES PICS SUR LE VECTEUR REARANGE
C
C -       LE PREMIER POINT EST UN PIC
          NPIC   = 1
          PIC(1) = RTRV(1)
          PINTER = RTRV(2)
C -       ON RECHERCHE TOUS LES PICS
            DO 1 I = 3,NTRV
            DP1 = PINTER  - PIC(NPIC)
            DP2 = RTRV(I) - PINTER
C -             ON CONSERVE LE POINT INTERMEDIAIRE COMME UN PIC
                IF( DP2*DP1 .LT.  0.D0 )  THEN
                NPIC = NPIC+1
                PIC(NPIC) = PINTER
                ENDIF
C -         LE DERNIER POINT DEVIENT POINT INTERMEDIAIRE
            PINTER = RTRV(I)
  1         CONTINUE
C -       LE DERNIER POINT EST UN PIC
          NPIC = NPIC+1
          PIC(NPIC) = RTRV(NTRV)
          ELSE
          K16B = METHOD(1:16)
          CALL UTMESS('F','FGPIC2','METHODE '//K16B//' ILLICITE')
          ENDIF
C
        END
