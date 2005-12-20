      SUBROUTINE RCADME ( NOMMAZ, PHENOM, NOMRES, VALRES, CODRET, STOP )
      IMPLICIT   NONE
      CHARACTER*(*)       NOMMAZ, PHENOM, NOMRES, STOP
      CHARACTER *2        CODRET
      INTEGER             VALRES(*)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 15/09/98   AUTEUR CIBHHLV L.VIVAN 
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
C     OBTENTION DES ADRESSES DES COMPOSANTES D'UN MATERIAU METALLURGIQUE
C
C     ARGUMENTS D'ENTREE:
C        NOMMAT : NOM UTILISATEUR DU MATERIAU
C     ARGUMENTS DE SORTIE:
C       VALRES : ADRESSE DU TRC IADTRC(1)=NBHIST IADTRC(2)=NBTRC
C       CODRET : 'OK' SI ON A TROUVE, 'NO' SINON
C ----------------------------------------------------------------------
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
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
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      INTEGER            IRET, IVALK, NBR, NBC, NBK, NBCO, IK, IADTRC,
     +                   NBCB1, NBCB2, NBLB2, NBHIST, NBTRC
      CHARACTER*8        NOMMAT, K8B
      CHARACTER*10       NOMPHE
      CHARACTER*16       TYPECO
      CHARACTER*19       CH19, LISTR
C DEB ------------------------------------------------------------------
C
      CALL JEMARQ()
      NOMMAT = NOMMAZ
      NOMPHE = PHENOM
C
      CALL JEEXIN ( NOMMAT//'.'//NOMPHE//'.VALR', IRET )
      IF ( IRET .EQ. 0 ) THEN
         CODRET = 'NO'
         GOTO 9999
      ELSE
        CALL JELIRA ( NOMMAT//'.'//NOMPHE//'.VALR', 'LONUTI', NBR, K8B)
      ENDIF
C
      CALL JEEXIN ( NOMMAT//'.'//NOMPHE//'.VALC', IRET )
      IF ( IRET .EQ. 0 ) THEN
         CODRET = 'NO'
         GOTO 9999
      ELSE
        CALL JELIRA ( NOMMAT//'.'//NOMPHE//'.VALC', 'LONUTI', NBC, K8B)
      ENDIF
C
      CALL JEEXIN ( NOMMAT//'.'//NOMPHE//'.VALK', IRET )
      IF ( IRET .EQ. 0 ) THEN
         CODRET = 'NO'
         GOTO 9999
      ELSE
        CALL JEVEUO ( NOMMAT//'.'//NOMPHE//'.VALK', 'L', IVALK)
        CALL JELIRA ( NOMMAT//'.'//NOMPHE//'.VALK', 'LONUTI', NBK, K8B)
      ENDIF
C
      NBCO = ( NBK - NBR - NBC ) / 2
      DO 150 IK = 1, NBK
         IF ( NOMRES .EQ. ZK8(IVALK+IK-1) ) THEN
            CODRET = 'OK'
            CH19   = ZK8(IVALK+NBCO+IK-1)
            CALL GETTCO ( CH19(1:8), TYPECO )
            IF ( TYPECO .EQ. 'TABL_TRC' ) THEN
               LISTR = '&&RCADME.LR8'
               CALL TBEXLR ( CH19, LISTR, 'V' )
               CALL JEVEUO ( LISTR//'.VALE' , 'L', IADTRC )
               NBCB1  = NINT( ZR(IADTRC+1) )
               NBHIST = NINT( ZR(IADTRC+2) )
               NBCB2  = NINT( ZR(IADTRC+1+2+NBCB1*NBHIST) )
               NBLB2  = NINT( ZR(IADTRC+1+2+NBCB1*NBHIST+1) )
               NBTRC =NINT(ZR(IADTRC+1+2+NBCB1*NBHIST+2+NBCB2*NBLB2+1))
C --- NBHIST
               VALRES(1) = NBHIST
C --- NBTRC
               VALRES(2) = NBTRC
               CALL JEDETC ( 'V', LISTR, 1 )
               GOTO 9999
            ELSE
               CALL UTMESS('F','RCADME','TYPE NON TRAITE: '//TYPECO)
            ENDIF
         ENDIF
  150 CONTINUE
C
 9999 CONTINUE
C
      CALL RCVALS( STOP, CODRET, 1, NOMRES )
C
      CALL JEDEMA()
C
      END
