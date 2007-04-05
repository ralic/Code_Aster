      SUBROUTINE TE0403 ( OPTION , NOMTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 06/04/2007   AUTEUR PELLET J.PELLET 
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
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*16    OPTION , NOMTE
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      INTEGER NB1
      REAL*8 VECL(51)
      REAL*8 VECTA(9,2,3),VECTN(9,3),VECTT(9,2,3),VECTPT(9,3,3)
      REAL*8       VALPAR(4), PR
      CHARACTER*8  NOMPAR(4), NOMAIL
      CHARACTER*24 VALK
C DEB ------------------------------------------------------------------
C
      CALL JEVECH ('PGEOMER' , 'L' , JGEOM)
C
      CALL JEVECH ('PVECTUR' , 'E' , JVECG)
C
      CALL JEVETE('&INEL.'//NOMTE(1:8)//'.DESR',' ', LZR )
C
      CALL JEVETE('&INEL.'//NOMTE(1:8)//'.DESI',' ', LZI )
      NB1  =ZI(LZI-1+1)
      NB2  =ZI(LZI-1+2)
      CALL VECTAN(NB1,NB2,ZR(JGEOM),ZR(LZR),VECTA,VECTN,VECTT)
C
      DO 5 IB=1,NB2
      DO 6  I=1,2
      DO 7  J=1,3
         VECTPT(IB,I,J)=VECTT(IB,I,J)
 7    CONTINUE
 6    CONTINUE
         VECTPT(IB,3,1)=VECTN(IB,1)
         VECTPT(IB,3,2)=VECTN(IB,2)
         VECTPT(IB,3,3)=VECTN(IB,3)
 5    CONTINUE
C
C     
      IF (OPTION.EQ.'CHAR_MECA_FRCO3D'.OR.
     &    OPTION.EQ.'CHAR_MECA_FFCO3D') THEN
C------------------------------------------------------
C      PAS DE CHANGEMENT DE SIGNE POUR LES FORCES REPARTIES  
C------------------------------------------------------
         CALL FSURF(OPTION,NOMTE,ZR(JGEOM),NB1,VECL,VECTPT)
C
      ELSE IF (OPTION.EQ.'CHAR_MECA_PESA_R') THEN
         CALL FPESA(NOMTE,ZR(JGEOM),NB1,VECL)
C
      ELSE IF (OPTION.EQ.'CHAR_MECA_ROTA_R') THEN
         CALL FCENT(NOMTE,ZR(JGEOM),NB1,VECL)
C
      ELSE IF (OPTION.EQ.'CHAR_MECA_PRES_R') THEN
C------------------------------------------------------
C      CHANGEMENT DE SIGNE POUR LES PRESSIONS DANS FPRES  
C------------------------------------------------------
         CALL FPRES(NOMTE,ZR(JGEOM),NB1,VECL,VECTPT)
C
      ELSE IF (OPTION.EQ.'CHAR_MECA_PRES_F') THEN
         CALL JEVECH ('PPRESSF', 'L', JPRES)
         CALL JEVECH ('PTEMPSR', 'L', ITEMPS)
         VALPAR(4) = ZR(ITEMPS)
         NOMPAR(4) = 'INST'
         NOMPAR(1) = 'X'
         NOMPAR(2) = 'Y'
         NOMPAR(3) = 'Z'
         DO 222 J = 0, NB1-1
            VALPAR(1) = ZR(JGEOM+3*J  )
            VALPAR(2) = ZR(JGEOM+3*J+1)
            VALPAR(3) = ZR(JGEOM+3*J+2)
            CALL FOINTE('FM',ZK8(JPRES),4,NOMPAR,VALPAR,PR,IER)
            IF ( PR .NE. 0.D0 ) THEN
               CALL TECAEL ( IADZI, IAZK24 )
               NOMAIL = ZK24(IAZK24-1+3)(1:8)
               VALK = NOMAIL
               CALL U2MESG('F', 'ELEMENTS4_92',1,VALK,0,0,0,0.D0)
            ENDIF
  222    CONTINUE
         GOTO 9999
      ENDIF
C
      CALL TRNFLG(NB2,VECTPT,VECL,ZR(JVECG))
C
 9999 CONTINUE
      END
