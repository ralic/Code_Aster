      SUBROUTINE RVECHB (EPSI,TYPMAI,NDFAC,R,VALCPM,NBCP,
     +                   NBSO,NBSI,VALCP)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 23/09/2005   AUTEUR CIBHHLV L.VIVAN 
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
C
      CHARACTER*8 TYPMAI
      INTEGER     NDFAC(*),NBCP,NBSO,NBSI
      REAL*8      EPSI,R(*),VALCPM(*),VALCP(*)
C
C**********************************************************************
C
C  OPERATION REALISEE
C  ------------------
C
C     EVALUATION DE CMP EN UM POINT D' UNE FACE
C
C  ARGUMENTS EN ENTREE
C  -------------------
C
C     EPSI   : PRECISION
C     TYPMAI : TYPE DE LA MAILLE
C     NDFAC  : NUMERO LOCAL DES NOEUD DE LA FACE (AVEC ORIENTATION)
C     R      : PARAMETRE(S) DE REPERAGE DU POINT SUR LA FACE
C     VALCPM : VALEUR DES CMP SUR LES NOEUD DE LA MAILLE
C              ORGANISATION SUIVANT LA NUMEROTATION LOCALE DES NOEUDS
C     NBCP   : NOMBRE DE CMP
C
C  ARGUMENTS EN ENTREE
C  -------------------
C
C     VALCP  : TABLE DES VALEURS DES CMPS AU POINT
C
C**********************************************************************
C
C  VARIABLES LOCALES
C  -----------------
      CHARACTER*2 DIM
      REAL*8      R1,R2
      INTEGER     ND,NF,NI,I
C
C================== CORPS DE LA ROUTINE ================================
C
C
      IF ( (TYPMAI .EQ. 'POI1') .OR.
     +     (TYPMAI .EQ.' SEG2') .OR.
     +     (TYPMAI .EQ.' SEG3') ) THEN
C
         DIM = '1D'
C
      ELSE IF ( (TYPMAI .EQ. 'TRIA3') .OR.
     +          (TYPMAI .EQ. 'TRIA6') .OR.
     +          (TYPMAI .EQ. 'QUAD4') .OR.
     +          (TYPMAI .EQ. 'QUAD8') .OR.
     +          (TYPMAI .EQ. 'QUAD9') ) THEN
C
         DIM = '2D'
C
      ELSE
C
         DIM = '3D'
C
      ENDIF
C
      LNG = NBSI*NBCP
C
      IF ( DIM .EQ. '1D' ) THEN
C
         ND = NDFAC(1)
C
         DO 10, I = 1, NBCP*NBSO, 1
C
            VALCP(I) = VALCPM(I + (ND-1)*LNG)
C
10       CONTINUE
C
      ELSE IF ( DIM .EQ. '2D' ) THEN
C
         ND = NDFAC(1)
         NF = NDFAC(2)
         NI = NDFAC(3)
         R1 = R(1)
C
         IF ( ABS(1.0D0 - R1) .LE. EPSI ) THEN
C
C           /* LE POINT EST LE NOEUD EXTREMITE DE LA FACE */
C
            DO 21, I = 1, NBSO*NBCP, 1
C
               VALCP(I) = VALCPM(I + (NF-1)*LNG)
C
21          CONTINUE
C
         ELSE IF ( ABS(R1) .LE. EPSI ) THEN
C
C           /* LE POINT EST LE NOEUD ORIGINE DE LA FACE */
C
            DO 32, I = 1, NBCP*NBSO, 1
C
               VALCP(I) = VALCPM(I + (ND-1)*LNG)
C
32          CONTINUE
C
         ELSE
C
C           /* LE POINT N'EST PAS AU BORD DE LA FACE */
C
            IF ( NI .EQ. 0 ) THEN
C
C           /* PAS DE NOEUD INTERMEDIAIRE : INTERPOLATION DE DEGRE 1 */
C
               DO 41, I = 1, NBCP*NBSO, 1
C
                  VALCP(I) = (1.0D0 - R1)*VALCPM(I + (ND-1)*LNG) +
     +                        R1*VALCPM(I + (NF-1)*NBCP*NBSI)
C
41             CONTINUE
C
            ELSE
C
C           /* NOEUD INTERMEDIAIRE : INTERPOLATION DE DEGRE 2 */
C
               DO 52, I = 1, NBCP*NBSO, 1
C
                  IF ( VALCPM(I+(NI-1)*LNG) .EQ. R8VIDE() ) THEN
                     VALCP(I) = (1.0D0 - R1)*VALCPM(I + (ND-1)*LNG) +
     +                          R1*VALCPM(I + (NF-1)*NBCP*NBSI)
                  ELSE
                     VALCP(I) = VALCPM(I + (ND-1)*LNG) +
     +                        R1*( (4.0D0*VALCPM(I + (NI-1)*LNG) -
     +                              3.0D0*VALCPM(I + (ND-1)*LNG) -
     +                                    VALCPM(I + (NF-1)*LNG))+
     +                              2.0D0*R1*( VALCPM(I + (NF-1)*LNG)+
     +                                         VALCPM(I + (ND-1)*LNG)-
     +                                         2.0D0*
     +                                         VALCPM(I + (NI-1)*LNG)))
                  ENDIF
C
52             CONTINUE
C
            ENDIF
C
         ENDIF
C
      ELSE
C
C        /* TRAITEMENT DU 3D ... */
C
      ENDIF
C
      END
