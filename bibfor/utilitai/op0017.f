      SUBROUTINE OP0017 ( IER )
      IMPLICIT REAL*8 (A-H,O-Z)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 09/01/2006   AUTEUR VABHHTS J.PELLET 
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
C     COMMANDE:  IMPR_CO
C
C     ------------------------------------------------------------------
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C
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
      LOGICAL       LATTR, LCONT, ULEXIS
      CHARACTER*1   BASE
      CHARACTER*8   NOMCO
      CHARACTER*16  NOMFI
      CHARACTER*72  CHAINE
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFMAJ()
C
      NIVO = 0
      CALL GETVIS ( ' ', 'NIVEAU', 1,1,1, NIVO, N3 )
C
      CALL GETVTX(' ','ATTRIBUT',0,1,1,CHAINE,N3)
      IF (CHAINE(1:3).EQ.'OUI') THEN
         LATTR = .TRUE.
      ELSE
         LATTR = .FALSE.
      END IF
C
      CALL GETVTX(' ','CONTENU',0,1,1,CHAINE,N3)
      IF (CHAINE(1:3).EQ.'OUI') THEN
         LCONT = .TRUE.
      ELSE
         LCONT = .FALSE.
      END IF
C
      CALL GETVTX(' ','BASE',0,1,1,CHAINE,N1)
      BASE=CHAINE(1:1)
C
      IFI = 0
      NOMFI = ' '
      CALL GETVIS ( ' ', 'UNITE'  , 1,1,1, IFI  , N2 )
      IF ( .NOT. ULEXIS( IFI ) ) THEN
         CALL ULOPEN ( IFI, ' ', NOMFI, 'NEW', 'O' )
      ENDIF
  
      CALL GETVID(' ','CO',0,1,0,NOMCO,N1)
      NBCO= -N1
      IF (NBCO.GT.0) THEN
        CALL WKVECT ('&&OP0017.LISTE_CO','V V K8',NBCO,IALICO)
        CALL GETVID(' ','CO',0,1,NBCO,ZK8(IALICO),N1)
        DO 1, I=1,NBCO
          CALL UTIMSD(IFI,NIVO,LATTR,LCONT,ZK8(IALICO-1+I),1,BASE)
 1      CONTINUE
      END IF
  
      CALL GETVTX(' ','CHAINE',0,1,1,CHAINE,N2)
      IF (N2.GT.0) THEN
         CALL GETLTX(' ','CHAINE',0,1,1,LONG,N3)
         CALL GETVIS(' ','POSITION',0,1,1,IPOS,N4)
         CALL UTIMSD(IFI,NIVO,LATTR,LCONT,CHAINE(1:LONG),IPOS,BASE)
      END IF
  
      CALL GETVTX(' ','TOUT',0,1,1,CHAINE,N2)
      IF (N2.GT.0) THEN
         CALL UTIMSD(IFI,NIVO,LATTR,LCONT,' ',0,BASE)
      END IF
  
      CALL JEDEMA()
      END
