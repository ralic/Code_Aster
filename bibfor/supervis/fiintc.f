      SUBROUTINE FIINTC ( CODMES, NOMF, NTERM, SVPAR, VALPU, RESU,IER)
      IMPLICIT NONE
      INTEGER           NTERM, SVPAR(*), IER
      CHARACTER*(*)     CODMES, NOMF
      REAL*8            VALPU(*), RESU(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 18/09/2002   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     CALCUL DE RESU = F(X,Y,Z,...)
C     F (DE NOM NOMF) EST UNE FONCTION INTERPRETABLE.
C     ------------------------------------------------------------------
C IN  CODMES : 'F','E','A','I',... PARAMETRE TRANSMIT A UTMESS.
C IN  NOMF   : NOM DE LA FONCTION OU DE LA NAPPE
C IN  NBPU   : NOMBRE DE PARAMETRES DANS NOMPU ET VALPU
C IN  NOMPU  : NOMS DES PARAMETRES "UTILISATEUR"
C IN  VALPU  : VALEURS DES PARAMETRES "UTILISATEUR"
C OUT RESU   : R : RESULTAT DE L'INTERPOLATION
C OUT IER    : CODE RETOUR
C     ------------------------------------------------------------------
      INTEGER      MXTERM, ICLASS, I, IVAL, IPLACE, 
     +             IPARG,IPARD,IVIRG,IEGAL,IPTVI,ILOAD,IPLUS,IFONC
      REAL*8       RVAL(2), LRVAL(2)
      CHARACTER*8  K8B
      PARAMETER  ( MXTERM = 10)
C     ------------------------------------------------------------------
      COMMON /FISY01/ IPARG,IPARD,IVIRG,IEGAL,IPTVI,ILOAD,IPLUS,IFONC
C     ------------------------------------------------------------------
      RESU(1) = 0.D0
      RESU(2) = 0.D0
C
      IER    = 0
      IF ( NTERM .GT. MXTERM ) THEN
         CALL UTDEBM('F','FIINTC','ERREUR  ')
         CALL UTIMPK('L','LA FONCTION ',1,NOMF)
         CALL UTIMPI('S',' A ',1,NTERM)
         CALL UTIMPI('S',' ARGUMENTS. LE MAXIMUM EXPLOITABLE EST ',
     +                                                        1,MXTERM)
         CALL UTFINM()
      ENDIF
C
      ICLASS = 35
      IVAL   = NTERM
      CALL FIREMP ( 0, ICLASS, IVAL, RVAL, NOMF, IPLACE )
      IPLACE = ABS(IPLACE)
C
      ICLASS = 15
      IVAL   = 0
      LRVAL(2) = 0.D0
      K8B   = '  '
      DO 10 I= 1, NTERM
         IPLACE = IPLACE + 1
         LRVAL(1)  = VALPU(SVPAR(I))
         CALL FIREMP ( 4, ICLASS, IVAL, LRVAL, K8B, IPLACE )
 10   CONTINUE
C
C     ------------------------------------------------------------------
      CALL FIOPER ( CODMES, NOMF, ICLASS, IVAL, RESU, IER )
C
      END
