      SUBROUTINE NMINI0(ZFON  ,ZPMET ,ZPCRI ,ZCONV ,ZPCON ,
     &                  ZNMETH,ZLICC ,FONACT,PARMET,PARCRI,
     &                  CONV  ,PARCON,METHOD,LICCVG,ETA   ,  
     &                  NUMINS,MTCPUI,MTCPUP,FINPAS)
C     
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/09/2008   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C RESPONSABLE MABBAS M.ABBAS
C TOLE CRP_21
C 
      IMPLICIT NONE
      INTEGER      ZFON  ,ZPMET ,ZPCRI ,ZCONV
      INTEGER      ZPCON ,ZNMETH,ZLICC
      LOGICAL      FONACT(ZFON)
      REAL*8       PARMET(ZPMET),PARCRI(ZPCRI),CONV(ZCONV)
      REAL*8       PARCON(ZPCON)  
      CHARACTER*16 METHOD(ZNMETH)  
      INTEGER      LICCVG(ZLICC)         
      LOGICAL      MTCPUI, MTCPUP, FINPAS
      INTEGER      NUMINS
      REAL*8       ETA
C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (INITIALISATIONS)
C
C PREMIERES INITIALISATIONS DE MECA_NON_LINE: MISES A ZERO
C      
C ----------------------------------------------------------------------
C
C
C
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
C
      INTEGER ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C
      REAL*8       ZERO
      PARAMETER    (ZERO=0.D0)
      REAL*8       R8VIDE
      INTEGER      I
C
C ----------------------------------------------------------------------
C 
C
C --- INITIALISATION DES INDICATEURS DE CONVERGENCE
C 
C              LICCVG(1) : PILOTAGE
C                        =  0 CONVERGENCE
C                        =  1 PAS DE CONVERGENCE
C                        = -1 BORNE ATTEINTE
C              LICCVG(2) : INTEGRATION DE LA LOI DE COMPORTEMENT
C                        = 0 OK
C                        = 1 ECHEC DANS L'INTEGRATION : PAS DE RESULTATS
C                        = 3 SIZZ NON NUL (DEBORST) ON CONTINUE A ITERER
C              LICCVG(3) : TRAITEMENT DU CONTACT 
C                        = 0 OK
C                        = 1 ECHEC (ITER > 2*NBLIAI+1)
C              LICCVG(4) : MATRICE DE CONTACT
C                        = 0 OK
C                        = 1 MATRICE DE CONTACT SINGULIERE
C              LICCVG(5) : MATRICE DU SYSTEME (MATASS)
C                        = 0 OK
C                        = 1 MATRICE SINGULIERE
      DO 1 I=1,ZLICC
        LICCVG(I) = 0
  1   CONTINUE
C
C --- FONCTIONNALITES ACTIVEES               (NMFONC/ISFONC)
C
      DO 2 I=1,ZFON
        FONACT(I) = .FALSE.
  2   CONTINUE
C
C --- PARAMETRES DES METHODES DE RESOLUTION  (NMDOMT)
C
      DO 3 I=1,ZPMET
        PARMET(I) = ZERO
  3   CONTINUE  
C
C --- PARAMETRES DES CRITERES DE CONVERGENCE (NMLECT)
C
      DO 4 I=1,ZPCRI
        PARCRI(I) = ZERO
  4   CONTINUE   
C
C --- INFORMATIONS SUR LA CONVERGENCE DU CALCUL
C
      DO 5 I=1,ZCONV
        CONV  (I) = R8VIDE()
  5   CONTINUE 
      CONV(3)  = -1
      CONV(10) = -1
C
C --- PARAMETRES DU CRITERE DE CONVERGENCE EN CONTRAINTE (NMLECT)
C  
      DO 7 I=1,ZPCON
        PARCON(I) = ZERO
  7   CONTINUE  
C
C --- METHODES DE RESOLUTION
C  
      DO 8 I=1,ZNMETH
        METHOD(I) = ' '
  8   CONTINUE       
C
C --- INITIALISATION BOUCLE EN TEMPS
C
      NUMINS = 0
      MTCPUI = .FALSE.
      MTCPUP = .FALSE.
      ETA    = 0.D0
      FINPAS = .FALSE.  
C
      END
