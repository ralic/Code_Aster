      SUBROUTINE RECTFC(NBMODE,NBVECT,OMESHI,NPIVOT,NBLAGR,VALPRO,
     +                  NVPRO,RESUFI,RESUFR,NFREQ)
      IMPLICIT   NONE
      INTEGER           NBMODE,NBVECT,NPIVOT,NBLAGR,NVPRO,NFREQ
      INTEGER           RESUFI(NFREQ,*)
      COMPLEX*16        OMESHI,VALPRO(NVPRO)
      REAL*8            RESUFR(NFREQ,*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 24/02/2003   AUTEUR NICOLAS O.NICOLAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
C     RECTIFIE LES VALEURS PROPRES COMPLEXES
C     ------------------------------------------------------------------
C     IN  : NBMODE  : NOMBRE DE MODE DEMANDES
C     IN  : NBVECT  : NOMBRE DE VECTEURS UTILISES AU COURS DU CALCUL
C     IN  : OMESHI  : DECALAGE UTILISE POUR LE CALCUL
C     IN  : NPIVOT  : NOMBRE DE PIVOTS NEGATIFS, POUR RECTIFIER LA
C                     POSITION DES MODES
C     IN  : NBLAGR  : NOMBRE DE PARAMETRES DE LAGRANGE
C     IN  : VALPRO  : VALEURS PROPRES
C     IN  : NVPRO   : DIMENSION DU VECTEUR VALPRO
C     OUT : RESUFI  : ON RANGE DANS LA STRUCTURE RESULTAT
C     OUT : RESUFR  : ON RANGE DANS LA STRUCTURE RESULTAT
C     IN  : NFREQ   : PREMIERE DIMENSION DU TABLEAU RESUFR
C     ------------------------------------------------------------------
      INTEGER      INEG, IP, IM, IN, IVEC, IFREQ
      COMPLEX*16   OM
C     ------------------------------------------------------------------
C
C     ------------------------------------------------------------------
C     --------  RECTIFICATION DES FREQUENCES DUE AU SHIFT  -------------
C     --------     DETERMINATION DE LA POSITION MODALE     -------------
C     ------------------------------------------------------------------
C
      INEG = 0
      IP   = 0
      IM   = 1
      DO 10 IVEC = 1,NBVECT
         OM = VALPRO(IVEC)
         IF (DBLE(OM).GT.0.0D0) THEN
            IP = IP + 1
            IN = IP
         ELSE
            IM = IM - 1
            IN = IM
         ENDIF
C
         OM =  OM - OMESHI
         IF (DBLE(OM).LT.0.0D0) THEN
              INEG = INEG + 1
         ENDIF
         IF (IVEC.LE.NBMODE) THEN
            RESUFI(IVEC,1) = NPIVOT+IN
            RESUFR(IVEC,2) = DBLE(OM)
            RESUFR(IVEC,3) = (DIMAG(OM) / DBLE(OM)) /2.D0
         ENDIF
   10 CONTINUE
      IF ( INEG .EQ. NBVECT ) THEN
         DO 20 IVEC = 1,NBMODE
            RESUFI(IVEC,1) = NPIVOT + IVEC
   20    CONTINUE
      ENDIF
C
C     ------------------------------------------------------------------
C     -- RECTIFICATION DE LA POSITION MODALE (A CAUSE DES LAGRANGE) ----
C     ------------------------------------------------------------------
C
      DO 30 IFREQ = 1, NBMODE
         RESUFI(IFREQ,1) = RESUFI(IFREQ,1) - NBLAGR
   30 CONTINUE
C
      END
