      SUBROUTINE CRNSLV(NUZ,METRES,RENUM,BASE)
      IMPLICIT  NONE
      CHARACTER*(*) NUZ,BASE,METRES,RENUM
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ASSEMBLA  DATE 28/02/2006   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
C---------------------------------------------------------
C  BUT : ENRICHIR LA SD NUME_DDL (NUZ) DE L'OBJET .NSLV
C        ET CREATION D'UNE SD SOLVEUR
C---------------------------------------------------------
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
      INTEGER     JNSLV
      CHARACTER*1 BAS1
      CHARACTER*14  NU
      CHARACTER*19  SOLVEU

C
      NU=NUZ
      BAS1=BASE

C     -- CREATION D'UNE SD SOLVEUR :
      SOLVEU=NU//'.SOLV'
      CALL CRSOLV(METRES,RENUM,SOLVEU,BAS1)

C --- CREATION DE L'OBJET .NSLV :
      CALL WKVECT(NU//'.NSLV',BAS1//' V K24',1,JNSLV)
      ZK24(JNSLV-1+1)=SOLVEU
      END
