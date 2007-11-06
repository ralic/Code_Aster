      SUBROUTINE GFCOPY ( CHGRFL, CHGRF2 )
      IMPLICIT NONE
      CHARACTER*24        CHGRFL, CHGRF2
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 06/11/2007   AUTEUR BOYERE E.BOYERE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C COPIE D'UNE SD DE GRAPPE FLUIDE DANS UNE AUTRE
C IN  : CHGRFL, SD DE GRAPPE FLUIDE A COPIER
C OUT : CHGRF2, SD DE GRAPPE FLUIDE COPIEE

C.========================= DEBUT DES DECLARATIONS ====================
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
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
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      INTEGER      I, NBNO, JIFL, JFFL, JFF2, IRET

C.========================= DEBUT DU CODE EXECUTABLE ==================
C
      CALL JEMARQ()
C
C RECUPERATION DE LA SD A COPIER
      CALL JEEXIN('&&GFLECT.INDICE',IRET)
      IF (IRET.EQ.0) GOTO 999
      CALL JEVEUO ( '&&GFLECT.INDICE', 'L', JIFL ) 
      CALL JEVEUO ( CHGRFL, 'L', JFFL ) 
      CALL JEVEUO ( CHGRF2, 'E', JFF2 ) 

      NBNO = ZI(JIFL-1+5)
C

C RECOPIE
      DO 111 I=1, 5*NBNO+1000
         ZR(JFF2-1+I) = ZR(JFFL-1+I)
 111  CONTINUE


 999  CONTINUE

      CALL JEDEMA()
C.============================ FIN DE LA ROUTINE ======================
      END
