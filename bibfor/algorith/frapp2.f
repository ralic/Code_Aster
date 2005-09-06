      SUBROUTINE FRAPP2(SD1,NOMA,LIGREL,CARTE,INST)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 06/09/2005   AUTEUR TORKHANI M.TORKHANI 
C TOLE CRP_20
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
C TOLE CRP_20
C      
      IMPLICIT NONE
      CHARACTER*8 NOMA
      CHARACTER*19 LIGREL,CARTE
      CHARACTER*24 SD1
      REAL*8 INST(5)

C    BUT : FABRIQUER UN LIGREL ET UNE CARTE
C   IN/JXIN  SD1 : SD1(DEFICO) =>TABFIN  CARACF
C   IN/JXIN  NOMA (K8) : SD MAILLAGE
C   IN/JXOUT LIGREL    : SD LIGREL PRODUITE
C   IN/JXOUT CARTE     : SD CARTE PRODUITE

C---------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      COMMON /IVARJE/ZI(1)
      COMMON /RVARJE/ZR(1)
      COMMON /CVARJE/ZC(1)
      COMMON /LVARJE/ZL(1)
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      INTEGER ZI
      REAL*8 ZR
      COMPLEX*16 ZC
      LOGICAL ZL,INTERN,STANDR
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32,JEXNOM,JEXNUM,JEXATR
      CHARACTER*80 ZK80
C---------------- COMMUNS NORMALISES  JEVEUX  --------------------------

      INTEGER NBTYP,ICO,JCO,NBGREL,IPC,NBPC,K,INO,NBNO1,NBNO2
      INTEGER JNCMP,JVALV,IZONE,JECPD
      INTEGER JNOMA,JTYMAI,NBNOT,JTYNMA,JTABF,IACNX1,ILCNX1,JCMCF
      INTEGER NUMA1,NUMA2,ITYMA1,ITYMA2
      INTEGER JNBNO,LONG,NBNO,JAD,ITYTE,ITYMA,NCMP
      PARAMETER (NBTYP=29)

      INTEGER COMPT(NBTYP)
      CHARACTER*2 CH2
      CHARACTER*8 KBID,NTYMA1,NTYMA2
      CHARACTER*16 NOMTE(NBTYP)
C     COMPT(1) : NOMBRE DE  CFS2S2
C     COMPT(2) : NOMBRE DE  CFS3S3
C     COMPT(3) : NOMBRE DE  CFS2S3
C     COMPT(4) : NOMBRE DE  CFS3S2
C     COMPT(5)  =NOMBRE DE  CFT3T3
C     COMPT(6)  =NOMBRE DE  CFT3T6
C     COMPT(7)  =NOMBRE DE  CFT6T3
C     COMPT(8)  =NOMBRE DE  CFT6T6
C     COMPT(9)  =NOMBRE DE  CFQ4Q4
C     COMPT(10) =NOMBRE DE  CFQ4Q8
C     COMPT(11) =NOMBRE DE  CFQ8Q4
C     COMPT(12) =NOMBRE DE  CFQ8Q8
C     COMPT(13) =NOMBRE DE  CFQ4T3
C     COMPT(14) =NOMBRE DE  CFT3Q4
C     COMPT(15) =NOMBRE DE  CFT6Q4
C     COMPT(16) =NOMBRE DE  CFQ4T6
C     COMPT(17) =NOMBRE DE  CFT6Q8
C     COMPT(18) =NOMBRE DE  CFQ8T6
C     COMPT(19) =NOMBRE DE  CFT6Q9
C     COMPT(20) =NOMBRE DE  CFQ9T6
C     COMPT(21) =NOMBRE DE  CFQ8T3
C     COMPT(22) =NOMBRE DE  CFT3Q8
C     COMPT(23) =NOMBRE DE  CFQ8Q9
C     COMPT(24) =NOMBRE DE  CFQ9Q8
C     COMPT(25) =NOMBRE DE  CFQ9Q4
C     COMPT(26) =NOMBRE DE  CFQ4Q9
C     COMPT(27) =NOMBRE DE  CFQ9T3
C     COMPT(28) =NOMBRE DE  CFT3Q9
C     COMPT(29) =NOMBRE DE  CFQ9Q9
      CALL JEMARQ()

C     1. DESTRUCTION DU LIGREL ET DE LA CARTE S'ILS EXISTENT :
C      ------------------------------------------------------
      CALL DETRSD('LIGREL',LIGREL)
      CALL DETRSD('CARTE',CARTE)

      NOMTE(1) = 'CFS2S2'
      NOMTE(2) = 'CFS3S3'
      NOMTE(3) = 'CFS2S3'
      NOMTE(4) = 'CFS3S2'
      NOMTE(5) = 'CFT3T3'
      NOMTE(6) = 'CFT3T6'
      NOMTE(7) = 'CFT6T3'
      NOMTE(8) = 'CFT6T6'
      NOMTE(9) = 'CFQ4Q4'
      NOMTE(10) = 'CFQ4Q8' 
      NOMTE(11) = 'CFQ8Q4' 
      NOMTE(12) = 'CFQ8Q8'
      NOMTE(13) = 'CFQ4T3'
      NOMTE(14) = 'CFT3Q4'
      NOMTE(15) = 'CFT6Q4'
      NOMTE(16) = 'CFQ4T6'
      NOMTE(17) = 'CFT6Q8'
      NOMTE(18) = 'CFQ8T6'
      NOMTE(19) = 'CFT6Q9'
      NOMTE(20) = 'CFQ9T6'
      NOMTE(21) = 'CFQ8T3'
      NOMTE(22) = 'CFT3Q8'
      NOMTE(23) = 'CFQ8Q9'
      NOMTE(24) = 'CFQ9Q8'
      NOMTE(25) = 'CFQ9Q4'
      NOMTE(26) = 'CFQ4Q9'
      NOMTE(27) = 'CFQ9T3'
      NOMTE(28) = 'CFT3Q9'
      NOMTE(29) = 'CFQ9Q9'


C     2.CREATION DU LIGREL :
C      --------------------


C     2.1 CREATION DE .NOMA :
C     ----------------------
      CALL WKVECT(LIGREL//'.NOMA','V V K8',1,JNOMA)
      ZK8(JNOMA-1+1) = NOMA

      CALL JEVEUO(NOMA//'.CONNEX','L',IACNX1)
      CALL JEVEUO(JEXATR(NOMA//'.CONNEX','LONCUM'),'L',ILCNX1)


C     2.2. ON COMPTE DES CHOSES ET ON CREE .TYPNEMA :
C      ------------------------------------------------
      DO 10,K = 1,29
        COMPT(K) = 0
   10 CONTINUE

      CALL JEVEUO(SD1(1:16)//'.TABFIN','L',JTABF)
      CALL JEVEUO(SD1(1:16)//'.CARACF','L',JCMCF)
      CALL JEVEUO(SD1(1:16)//'.ECPDON','L',JECPD)
      CALL JEVEUO(NOMA//'.TYPMAIL','L',JTYMAI)
      NBPC = NINT(ZR(JTABF-1+1))


      CALL WKVECT('&&FRAPP2.TYPNEMA','V V I',2*NBPC,JTYNMA)
C          : 2 ENTIERS PAR NOUVELLE MAILLE :
C              NUMERO DU TYPE DE MAILLE
C              NOMBRE DE NOEUDS DU TYPE DE MAILLE
      NBNOT = 0
      DO 20,IPC = 1,NBPC
   
        NUMA1 = NINT(ZR(JTABF+21* (IPC-1)+1))
        NUMA2 = NINT(ZR(JTABF+21* (IPC-1)+2))
        ITYMA1 = ZI(JTYMAI-1+NUMA1)
        ITYMA2 = ZI(JTYMAI-1+NUMA2)



        CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ITYMA1),NTYMA1)
        CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ITYMA2),NTYMA2)         
        IF ((NTYMA1.EQ.'SEG2') .AND. (NTYMA2.EQ.'SEG2')) THEN

            CALL JENONU(JEXNOM('&CATA.TM.NOMTM','SEG22'),
     &                  ZI(JTYNMA-1+2* (IPC-1)+1))
            ZI(JTYNMA-1+2* (IPC-1)+2) = 4
            NBNOT = NBNOT + 0
            COMPT(1) = COMPT(1) + 1
         
        ELSE IF ((NTYMA1.EQ.'SEG3') .AND. (NTYMA2.EQ.'SEG3')) THEN

            CALL JENONU(JEXNOM('&CATA.TM.NOMTM','SEG33'),
     &                  ZI(JTYNMA-1+2* (IPC-1)+1))
            ZI(JTYNMA-1+2* (IPC-1)+2) = 6
            NBNOT = NBNOT + 0
            COMPT(2) = COMPT(2) + 1
          
        ELSE IF ((NTYMA1.EQ.'SEG2') .AND. (NTYMA2.EQ.'SEG3')) THEN

            CALL JENONU(JEXNOM('&CATA.TM.NOMTM','SEG23'),
     &                  ZI(JTYNMA-1+2* (IPC-1)+1))
            ZI(JTYNMA-1+2* (IPC-1)+2) = 5
            NBNOT = NBNOT + 0
            COMPT(3) = COMPT(3) + 1
        
        ELSE IF ((NTYMA1.EQ.'SEG3') .AND. (NTYMA2.EQ.'SEG2')) THEN

            CALL JENONU(JEXNOM('&CATA.TM.NOMTM','SEG32'),
     &                  ZI(JTYNMA-1+2* (IPC-1)+1))
            ZI(JTYNMA-1+2* (IPC-1)+2) = 5
            NBNOT = NBNOT + 0
            COMPT(4) = COMPT(4) + 1
          
        ELSE IF ((NTYMA1.EQ.'TRIA3') .AND. (NTYMA2.EQ.'TRIA3')) THEN

            CALL JENONU(JEXNOM('&CATA.TM.NOMTM','TRIA33'),
     &                  ZI(JTYNMA-1+2* (IPC-1)+1))
            ZI(JTYNMA-1+2* (IPC-1)+2) = 6
            NBNOT = NBNOT + 0
            COMPT(5) = COMPT(5) + 1
        
        ELSE IF ((NTYMA1.EQ.'TRIA3') .AND. (NTYMA2.EQ.'TRIA6')) THEN

            CALL JENONU(JEXNOM('&CATA.TM.NOMTM','TR3TR6'),
     &                  ZI(JTYNMA-1+2* (IPC-1)+1))
            ZI(JTYNMA-1+2* (IPC-1)+2) = 9
            NBNOT = NBNOT + 0
            COMPT(6) = COMPT(6) + 1

        ELSE IF ((NTYMA1.EQ.'TRIA6') .AND. (NTYMA2.EQ.'TRIA3')) THEN

            CALL JENONU(JEXNOM('&CATA.TM.NOMTM','TR6TR3'),
     &                  ZI(JTYNMA-1+2* (IPC-1)+1))
            ZI(JTYNMA-1+2* (IPC-1)+2) = 9
            NBNOT = NBNOT + 0
            COMPT(7) = COMPT(7) + 1
          
        ELSE IF ((NTYMA1.EQ.'TRIA6') .AND. (NTYMA2.EQ.'TRIA6')) THEN

            CALL JENONU(JEXNOM('&CATA.TM.NOMTM','TRIA66'),
     &                  ZI(JTYNMA-1+2* (IPC-1)+1))
            ZI(JTYNMA-1+2* (IPC-1)+2) = 12
            NBNOT = NBNOT + 0
            COMPT(8) = COMPT(8) + 1
         
        ELSE IF ((NTYMA1.EQ.'QUAD4') .AND. (NTYMA2.EQ.'QUAD4')) THEN

           CALL JENONU(JEXNOM('&CATA.TM.NOMTM','QUAD44'),
     &                  ZI(JTYNMA-1+2* (IPC-1)+1))
            ZI(JTYNMA-1+2* (IPC-1)+2) = 8
            NBNOT = NBNOT + 0
            COMPT(9) = COMPT(9) + 1
          
        ELSE IF ((NTYMA1.EQ.'QUAD4') .AND. (NTYMA2.EQ.'QUAD8')) THEN

            CALL JENONU(JEXNOM('&CATA.TM.NOMTM','QU4QU8'),
     &                  ZI(JTYNMA-1+2* (IPC-1)+1))
            ZI(JTYNMA-1+2* (IPC-1)+2) = 12
            NBNOT = NBNOT + 0
            COMPT(10) = COMPT(10) + 1
          

        ELSE IF ((NTYMA1.EQ.'QUAD8') .AND. (NTYMA2.EQ.'QUAD4')) THEN

            CALL JENONU(JEXNOM('&CATA.TM.NOMTM','QU8QU4'),
     &                  ZI(JTYNMA-1+2* (IPC-1)+1))
            ZI(JTYNMA-1+2* (IPC-1)+2) = 12
            NBNOT = NBNOT + 0
            COMPT(11) = COMPT(11) + 1
         
        ELSE IF ((NTYMA1.EQ.'QUAD8') .AND. (NTYMA2.EQ.'QUAD8')) THEN
            
            CALL JENONU(JEXNOM('&CATA.TM.NOMTM','QUAD88'),
     &                  ZI(JTYNMA-1+2* (IPC-1)+1))
            ZI(JTYNMA-1+2* (IPC-1)+2) = 16
            NBNOT = NBNOT + 0
            COMPT(12) = COMPT(12) + 1
        ELSE IF ((NTYMA1.EQ.'QUAD4') .AND. (NTYMA2.EQ.'TRIA3')) THEN
            CALL JENONU(JEXNOM('&CATA.TM.NOMTM','QU4TR3'),
     &                  ZI(JTYNMA-1+2* (IPC-1)+1))
            ZI(JTYNMA-1+2* (IPC-1)+2) = 7
            NBNOT = NBNOT + 0
            COMPT(13) = COMPT(13) + 1
        ELSE IF ((NTYMA1.EQ.'TRIA3') .AND. (NTYMA2.EQ.'QUAD4')) THEN
            CALL JENONU(JEXNOM('&CATA.TM.NOMTM','TR3QU4'),
     &                  ZI(JTYNMA-1+2* (IPC-1)+1))
            ZI(JTYNMA-1+2* (IPC-1)+2) = 7
            NBNOT = NBNOT + 0
            COMPT(14) = COMPT(14) + 1
        ELSE IF ((NTYMA1.EQ.'TRIA6') .AND. (NTYMA2.EQ.'QUAD4')) THEN
            CALL JENONU(JEXNOM('&CATA.TM.NOMTM','TR6QU4'),
     &                  ZI(JTYNMA-1+2* (IPC-1)+1))
            ZI(JTYNMA-1+2* (IPC-1)+2) = 10
            NBNOT = NBNOT + 0
            COMPT(15) = COMPT(15) + 1
        ELSE IF ((NTYMA1.EQ.'QUAD4') .AND. (NTYMA2.EQ.'TRIA6')) THEN
            CALL JENONU(JEXNOM('&CATA.TM.NOMTM','QU4TR6'),
     &                  ZI(JTYNMA-1+2* (IPC-1)+1))
            ZI(JTYNMA-1+2* (IPC-1)+2) = 10
            NBNOT = NBNOT + 0
            COMPT(16) = COMPT(16) + 1
        ELSE IF ((NTYMA1.EQ.'TRIA6') .AND. (NTYMA2.EQ.'QUAD8')) THEN
            CALL JENONU(JEXNOM('&CATA.TM.NOMTM','TR6QU8'),
     &                  ZI(JTYNMA-1+2* (IPC-1)+1))
            ZI(JTYNMA-1+2* (IPC-1)+2) = 14
            NBNOT = NBNOT + 0
            COMPT(17) = COMPT(17) + 1
        ELSE IF ((NTYMA1.EQ.'QUAD8') .AND. (NTYMA2.EQ.'TRIA6')) THEN
            CALL JENONU(JEXNOM('&CATA.TM.NOMTM','QU8TR6'),
     &                  ZI(JTYNMA-1+2* (IPC-1)+1))
            ZI(JTYNMA-1+2* (IPC-1)+2) = 14
            NBNOT = NBNOT + 0
            COMPT(18) = COMPT(18) + 1          
        ELSE IF ((NTYMA1.EQ.'TRIA6') .AND. (NTYMA2.EQ.'QUAD9')) THEN
            CALL JENONU(JEXNOM('&CATA.TM.NOMTM','TR6QU9'),
     &                  ZI(JTYNMA-1+2* (IPC-1)+1))
            ZI(JTYNMA-1+2* (IPC-1)+2) = 15
            NBNOT = NBNOT + 0
            COMPT(19) = COMPT(19) + 1
        ELSE IF ((NTYMA1.EQ.'QUAD9') .AND. (NTYMA2.EQ.'TRIA6')) THEN
            CALL JENONU(JEXNOM('&CATA.TM.NOMTM','QU9TR6'),
     &                  ZI(JTYNMA-1+2* (IPC-1)+1))
            ZI(JTYNMA-1+2* (IPC-1)+2) = 15
            NBNOT = NBNOT + 0
            COMPT(20) = COMPT(20) + 1             
        ELSE IF ((NTYMA1.EQ.'QUAD8') .AND. (NTYMA2.EQ.'TRIA3')) THEN
            CALL JENONU(JEXNOM('&CATA.TM.NOMTM','QU8TR3'),
     &                  ZI(JTYNMA-1+2* (IPC-1)+1))
            ZI(JTYNMA-1+2* (IPC-1)+2) = 11
            NBNOT = NBNOT + 0
            COMPT(21) = COMPT(21) + 1
        ELSE IF ((NTYMA1.EQ.'TRIA3') .AND. (NTYMA2.EQ.'QUAD8')) THEN
            CALL JENONU(JEXNOM('&CATA.TM.NOMTM','TR3QU8'),
     &                  ZI(JTYNMA-1+2* (IPC-1)+1))
            ZI(JTYNMA-1+2* (IPC-1)+2) = 11
            NBNOT = NBNOT + 0
            COMPT(22) = COMPT(22) + 1  
        ELSE IF ((NTYMA1.EQ.'QUAD8') .AND. (NTYMA2.EQ.'QUAD9')) THEN

            CALL JENONU(JEXNOM('&CATA.TM.NOMTM','QU8QU9'),
     &                  ZI(JTYNMA-1+2* (IPC-1)+1))
            ZI(JTYNMA-1+2* (IPC-1)+2) = 17
            NBNOT = NBNOT + 0
            COMPT(23) = COMPT(23) + 1
         
        ELSE IF ((NTYMA1.EQ.'QUAD9') .AND. (NTYMA2.EQ.'QUAD8')) THEN

            CALL JENONU(JEXNOM('&CATA.TM.NOMTM','QU8QU8'),
     &                  ZI(JTYNMA-1+2* (IPC-1)+1))
            ZI(JTYNMA-1+2* (IPC-1)+2) = 17
            NBNOT = NBNOT + 0
            COMPT(24) = COMPT(24) + 1
        ELSE IF ((NTYMA1.EQ.'QUAD9') .AND. (NTYMA2.EQ.'QUAD4')) THEN

            CALL JENONU(JEXNOM('&CATA.TM.NOMTM','QU9QU4'),
     &                  ZI(JTYNMA-1+2* (IPC-1)+1))
            ZI(JTYNMA-1+2* (IPC-1)+2) = 13
            NBNOT = NBNOT + 0
            COMPT(25) = COMPT(25) + 1
         
        ELSE IF ((NTYMA1.EQ.'QUAD4') .AND. (NTYMA2.EQ.'QUAD9')) THEN

            CALL JENONU(JEXNOM('&CATA.TM.NOMTM','QU4QU9'),
     &                  ZI(JTYNMA-1+2* (IPC-1)+1))
            ZI(JTYNMA-1+2* (IPC-1)+2) = 13
            NBNOT = NBNOT + 0
            COMPT(26) = COMPT(26) + 1
        ELSE IF ((NTYMA1.EQ.'QUAD9') .AND. (NTYMA2.EQ.'TRIA3')) THEN

            CALL JENONU(JEXNOM('&CATA.TM.NOMTM','QU9TR3'),
     &                  ZI(JTYNMA-1+2* (IPC-1)+1))
            ZI(JTYNMA-1+2* (IPC-1)+2) = 12
            NBNOT = NBNOT + 0
            COMPT(27) = COMPT(27) + 1
         
        ELSE IF ((NTYMA1.EQ.'TRIA3') .AND. (NTYMA2.EQ.'QUAD9')) THEN

            CALL JENONU(JEXNOM('&CATA.TM.NOMTM','TR3QU9'),
     &                  ZI(JTYNMA-1+2* (IPC-1)+1))
            ZI(JTYNMA-1+2* (IPC-1)+2) = 12
            NBNOT = NBNOT + 0
            COMPT(28) = COMPT(28) + 1
        ELSE IF ((NTYMA1.EQ.'QUAD9') .AND. (NTYMA2.EQ.'QUAD9')) THEN

            CALL JENONU(JEXNOM('&CATA.TM.NOMTM','QUAD99'),
     &                  ZI(JTYNMA-1+2* (IPC-1)+1))
            ZI(JTYNMA-1+2* (IPC-1)+2) = 18
            NBNOT = NBNOT + 0
            COMPT(29) = COMPT(29) + 1
        ELSE
          CALL JXABOR()
        END IF


   20 CONTINUE

C     2.3 CREATION DE .NBNO :
C      --------------------
      CALL WKVECT(LIGREL//'.NBNO','V V I',1,JNBNO)
      ZI(JNBNO-1+1) = NBNOT


C     2.4 CREATION DE .NEMA :
C      --------------------
      CALL JECREC(LIGREL//'.NEMA','V V I','NU','CONTIG','VARIABLE',NBPC)
      LONG = 0
C    'CFS2S2'   :
      LONG = LONG + COMPT(1)* (4+1)
C     'CFS3S3'  :
      LONG = LONG + COMPT(2)* (6+1)
C     'CFS2S3'  :
      LONG = LONG + COMPT(3)* (5+1)
C     'CFS3S2'  :
      LONG = LONG + COMPT(4)* (5+1)
C     'CFT3T3' 
      LONG = LONG + COMPT(5)* (6+1)
C     'CFT3T6'                            
      LONG = LONG + COMPT(6)* (9+1)      
C     'CFT6T3'                             
      LONG = LONG + COMPT(7)* (9+1)      
C     'CFT6T6'                            
      LONG = LONG + COMPT(8)* (12+1)      
C     'CFQ4Q4'                                 
      LONG = LONG + COMPT(9)* (12+1)      
C     'CFQ4Q8'                                    
      LONG = LONG + COMPT(10)* (12+1)      
C     'CFQ8Q4'                              
      LONG = LONG + COMPT(11)* (12+1)      
C     'CFQ8Q8'                            
      LONG = LONG + COMPT(12)* (16+1)     
C     'CFQ4T3'                              
      LONG = LONG + COMPT(13)* (7+1)      
C     'CFT3Q4'                             
      LONG = LONG + COMPT(14)* (7+1)     
C     'CFT6Q4'                              
      LONG = LONG + COMPT(15)* (10+1)     
C     'CFQ4T6'                             
      LONG = LONG + COMPT(16)* (10+1)     
C     'CFT6Q8'                              
      LONG = LONG + COMPT(17)* (14+1)      
C     'CFQ8T6'                             
      LONG = LONG + COMPT(18)* (14+1)      
C     'CFT6Q9'                              
      LONG = LONG + COMPT(19)* (15+1)     
C     'CFQ9T6'                             
      LONG = LONG + COMPT(20)* (15+1)
C     'CFQ8T3' 
      LONG = LONG + COMPT(21)* (11+1)
C     'CFT3Q8' 
      LONG = LONG + COMPT(22)* (11+1)
C     'CFQ8Q9' 
      LONG = LONG + COMPT(23)* (17+1)
C     'CFQ9Q8' 
      LONG = LONG + COMPT(24)* (17+1)
C     'CFQ9Q4' 
      LONG = LONG + COMPT(25)* (13+1)
C     'CFQ4Q9' 
      LONG = LONG + COMPT(26)* (13+1)
C     'CFQ9T3'
      LONG = LONG + COMPT(27)* (12+1)
C     'CFT3Q9'
      LONG = LONG + COMPT(28)* (12+1)
C     'CFQ9Q9'
      LONG = LONG + COMPT(29)* (18+1)
C 
C 


      CALL JEECRA(LIGREL//'.NEMA','LONT',LONG,KBID)
      DO 50,IPC = 1,NBPC
        CALL JECROC(JEXNUM(LIGREL//'.NEMA',IPC))
        NBNO = ZI(JTYNMA-1+2* (IPC-1)+2)
        CALL JEECRA(JEXNUM(LIGREL//'.NEMA',IPC),'LONMAX',NBNO+1,KBID)
        CALL JEVEUO(JEXNUM(LIGREL//'.NEMA',IPC),'E',JAD)
        ZI(JAD-1+NBNO+1) = ZI(JTYNMA-1+2* (IPC-1)+1)

        NUMA1 = NINT(ZR(JTABF+21* (IPC-1)+1))
        NUMA2 = NINT(ZR(JTABF+21* (IPC-1)+2))
        NBNO1 = ZI(ILCNX1+NUMA1) - ZI(ILCNX1-1+NUMA1)
        NBNO2 = ZI(ILCNX1+NUMA2) - ZI(ILCNX1-1+NUMA2)

C       -- RECOPIE DES NUMEROS DE NOEUDS DE LA MAILLE ESCLAVE
        DO 30,INO = 1,NBNO1
          ZI(JAD-1+INO) = ZI(IACNX1+ZI(ILCNX1-1+NUMA1)-2+INO)
   30   CONTINUE

C       -- RECOPIE DES NUMEROS DE NOEUDS DE LA MAILLE MAITRE
        DO 40,INO = 1,NBNO2
          ZI(JAD-1+NBNO1+INO) = ZI(IACNX1+ZI(ILCNX1-1+NUMA2)-2+INO)
   40   CONTINUE

   50 CONTINUE



C      2.5 CREATION DE .LIEL:
C      --------------------
      NBGREL = 0
      DO 60,K = 1,NBTYP
        IF (COMPT(K).GT.0) NBGREL = NBGREL + 1
   60 CONTINUE
      IF (NBGREL.EQ.0) CALL JXABOR()
      CALL JECREC(LIGREL//'.LIEL','V V I','NU','CONTIG','VARIABLE',
     &            NBGREL)

      LONG = NBGREL
      DO 70,K = 1,NBTYP
        LONG = LONG + COMPT(K)
   70 CONTINUE
         
      CALL JEECRA(LIGREL//'.LIEL','LONT',LONG,KBID)
      ICO = 0
      DO 90,K = 1,NBTYP
        IF (COMPT(K).EQ.0) GO TO 90
        ICO = ICO + 1
        CALL JECROC(JEXNUM(LIGREL//'.LIEL',ICO))
        CALL JEECRA(JEXNUM(LIGREL//'.LIEL',ICO),'LONMAX',COMPT(K)+1,
     &              KBID)
        CALL JEVEUO(JEXNUM(LIGREL//'.LIEL',ICO),'E',JAD)

        IF (NOMTE(K).EQ.'CFS2S2') THEN
          CALL JENONU(JEXNOM('&CATA.TE.NOMTE','CFS2S2'),ITYTE)
          CALL JENONU(JEXNOM('&CATA.TM.NOMTM','SEG22'),ITYMA)
          ZI(JAD-1+COMPT(K)+1) = ITYTE
        ELSE IF (NOMTE(K).EQ.'CFS3S3') THEN
          CALL JENONU(JEXNOM('&CATA.TE.NOMTE','CFS3S3'),ITYTE)
          CALL JENONU(JEXNOM('&CATA.TM.NOMTM','SEG33'),ITYMA)
          ZI(JAD-1+COMPT(K)+1) = ITYTE
        ELSE IF (NOMTE(K).EQ.'CFS2S3') THEN
          CALL JENONU(JEXNOM('&CATA.TE.NOMTE','CFS2S3'),ITYTE)
          CALL JENONU(JEXNOM('&CATA.TM.NOMTM','SEG23'),ITYMA)
          ZI(JAD-1+COMPT(K)+1) = ITYTE
        ELSE IF (NOMTE(K).EQ.'CFS3S2') THEN
          CALL JENONU(JEXNOM('&CATA.TE.NOMTE','CFS3S2'),ITYTE)
          CALL JENONU(JEXNOM('&CATA.TM.NOMTM','SEG32'),ITYMA)
          ZI(JAD-1+COMPT(K)+1) = ITYTE
        ELSE IF (NOMTE(K).EQ.'CFT3T3') THEN
          CALL JENONU(JEXNOM('&CATA.TE.NOMTE','CFT3T3'),ITYTE)
          CALL JENONU(JEXNOM('&CATA.TM.NOMTM','TRIA33'),ITYMA)
          ZI(JAD-1+COMPT(K)+1) = ITYTE
        ELSE IF (NOMTE(K).EQ.'CFT3T6') THEN
          CALL JENONU(JEXNOM('&CATA.TE.NOMTE','CFT3T6'),ITYTE)
          CALL JENONU(JEXNOM('&CATA.TM.NOMTM','TR3TR6'),ITYMA)
          ZI(JAD-1+COMPT(K)+1) = ITYTE
        ELSE IF (NOMTE(K).EQ.'CFT6T3') THEN 
          CALL JENONU(JEXNOM('&CATA.TE.NOMTE','CFT6T3'),ITYTE)
          CALL JENONU(JEXNOM('&CATA.TM.NOMTM','TR6TR3'),ITYMA)
          ZI(JAD-1+COMPT(K)+1) = ITYTE        
        ELSE IF (NOMTE(K).EQ.'CFT6T6') THEN
          CALL JENONU(JEXNOM('&CATA.TE.NOMTE','CFT6T6'),ITYTE)
          CALL JENONU(JEXNOM('&CATA.TM.NOMTM','TRIA66'),ITYMA)
          ZI(JAD-1+COMPT(K)+1) = ITYTE    
        ELSE IF (NOMTE(K).EQ.'CFQ4Q4') THEN  
          CALL JENONU(JEXNOM('&CATA.TE.NOMTE','CFQ4Q4'),ITYTE)
          CALL JENONU(JEXNOM('&CATA.TM.NOMTM','QUAD44'),ITYMA) 
          ZI(JAD-1+COMPT(K)+1) = ITYTE 
        ELSE IF (NOMTE(K).EQ.'CFQ4Q8') THEN  
          CALL JENONU(JEXNOM('&CATA.TE.NOMTE','CFQ4Q8'),ITYTE)
          CALL JENONU(JEXNOM('&CATA.TM.NOMTM','QU4QU8'),ITYMA) 
          ZI(JAD-1+COMPT(K)+1) = ITYTE    
        ELSE IF (NOMTE(K).EQ.'CFQ8Q4') THEN     
          CALL JENONU(JEXNOM('&CATA.TE.NOMTE','CFQ8Q4'),ITYTE) 
          CALL JENONU(JEXNOM('&CATA.TM.NOMTM','QU8QU4'),ITYMA) 
          ZI(JAD-1+COMPT(K)+1) = ITYTE  
        ELSE IF (NOMTE(K).EQ.'CFQ8Q8') THEN    
          CALL JENONU(JEXNOM('&CATA.TE.NOMTE','CFQ8Q8'),ITYTE)
          CALL JENONU(JEXNOM('&CATA.TM.NOMTM','QUAD88'),ITYMA)
          ZI(JAD-1+COMPT(K)+1) = ITYTE   
        ELSE IF (NOMTE(K).EQ.'CFQ4T3') THEN  
          CALL JENONU(JEXNOM('&CATA.TE.NOMTE','CFQ4T3'),ITYTE) 
          CALL JENONU(JEXNOM('&CATA.TM.NOMTM','QU4TR3'),ITYMA)
          ZI(JAD-1+COMPT(K)+1) = ITYTE  
        ELSE IF (NOMTE(K).EQ.'CFT3Q4') THEN  
          CALL JENONU(JEXNOM('&CATA.TE.NOMTE','CFT3Q4'),ITYTE) 
          CALL JENONU(JEXNOM('&CATA.TM.NOMTM','TR3QU4'),ITYMA)  
          ZI(JAD-1+COMPT(K)+1) = ITYTE 
        ELSE IF (NOMTE(K).EQ.'CFT6Q4') THEN 
          CALL JENONU(JEXNOM('&CATA.TE.NOMTE','CFT6Q4'),ITYTE)
          CALL JENONU(JEXNOM('&CATA.TM.NOMTM','TR6QU4'),ITYMA)
          ZI(JAD-1+COMPT(K)+1) = ITYTE
        ELSE IF (NOMTE(K).EQ.'CFQ4T6') THEN
          CALL JENONU(JEXNOM('&CATA.TE.NOMTE','CFQ4T6'),ITYTE)
          CALL JENONU(JEXNOM('&CATA.TM.NOMTM','QU4TR6'),ITYMA)
          ZI(JAD-1+COMPT(K)+1) = ITYTE
        ELSE IF (NOMTE(K).EQ.'CFT6Q8') THEN
          CALL JENONU(JEXNOM('&CATA.TE.NOMTE','CFT6Q8'),ITYTE)
          CALL JENONU(JEXNOM('&CATA.TM.NOMTM','TR6QU8'),ITYMA)
          ZI(JAD-1+COMPT(K)+1) = ITYTE
        ELSE IF (NOMTE(K).EQ.'CFQ8T6') THEN
          CALL JENONU(JEXNOM('&CATA.TE.NOMTE','CFQ8T6'),ITYTE)
          CALL JENONU(JEXNOM('&CATA.TM.NOMTM','QU8TR6'),ITYMA)
          ZI(JAD-1+COMPT(K)+1) = ITYTE
        ELSE IF (NOMTE(K).EQ.'CFT6Q9') THEN
          CALL JENONU(JEXNOM('&CATA.TE.NOMTE','CFT6Q9'),ITYTE)
          CALL JENONU(JEXNOM('&CATA.TM.NOMTM','TR6QU9'),ITYMA)
          ZI(JAD-1+COMPT(K)+1) = ITYTE
        ELSE IF (NOMTE(K).EQ.'CFQ9T6') THEN
          CALL JENONU(JEXNOM('&CATA.TE.NOMTE','CFQ9T6'),ITYTE)
          CALL JENONU(JEXNOM('&CATA.TM.NOMTM','QU9TR6'),ITYMA) 
          ZI(JAD-1+COMPT(K)+1) = ITYTE 
        ELSE IF (NOMTE(K).EQ.'CFQ8T3') THEN  
          CALL JENONU(JEXNOM('&CATA.TE.NOMTE','CFQ8T3'),ITYTE)  
          CALL JENONU(JEXNOM('&CATA.TM.NOMTM','QU8TR3'),ITYMA)  
          ZI(JAD-1+COMPT(K)+1) = ITYTE
        ELSE IF (NOMTE(K).EQ.'CFT3Q8') THEN
          CALL JENONU(JEXNOM('&CATA.TE.NOMTE','CFT3Q8'),ITYTE)
          CALL JENONU(JEXNOM('&CATA.TM.NOMTM','TR3QU8'),ITYMA)
          ZI(JAD-1+COMPT(K)+1) = ITYTE
        ELSE IF (NOMTE(K).EQ.'CFQ8Q9') THEN
          CALL JENONU(JEXNOM('&CATA.TE.NOMTE','CFQ8Q9'),ITYTE)
          CALL JENONU(JEXNOM('&CATA.TM.NOMTM','QU8QU9'),ITYMA)
          ZI(JAD-1+COMPT(K)+1) = ITYTE
        ELSE IF (NOMTE(K).EQ.'CFQ9Q8') THEN
          CALL JENONU(JEXNOM('&CATA.TE.NOMTE','CFQ9Q8'),ITYTE)
          CALL JENONU(JEXNOM('&CATA.TM.NOMTM','QU9QU8'),ITYMA)
          ZI(JAD-1+COMPT(K)+1) = ITYTE
        ELSE IF (NOMTE(K).EQ.'CFQ9Q4') THEN
          CALL JENONU(JEXNOM('&CATA.TE.NOMTE','CFQ9Q4'),ITYTE)
          CALL JENONU(JEXNOM('&CATA.TM.NOMTM','QU9QU4'),ITYMA)
          ZI(JAD-1+COMPT(K)+1) = ITYTE
        ELSE IF (NOMTE(K).EQ.'CFQ4Q9') THEN
          CALL JENONU(JEXNOM('&CATA.TE.NOMTE','CFQ4Q9'),ITYTE)
          CALL JENONU(JEXNOM('&CATA.TM.NOMTM','QU4QU9'),ITYMA)
          ZI(JAD-1+COMPT(K)+1) = ITYTE
        ELSE IF (NOMTE(K).EQ.'CFQ9T3') THEN
          CALL JENONU(JEXNOM('&CATA.TE.NOMTE','CFQ9T3'),ITYTE)
          CALL JENONU(JEXNOM('&CATA.TM.NOMTM','QU9TR3'),ITYMA)
          ZI(JAD-1+COMPT(K)+1) = ITYTE
        ELSE IF (NOMTE(K).EQ.'CFT3Q9') THEN
          CALL JENONU(JEXNOM('&CATA.TE.NOMTE','CFT3Q9'),ITYTE)
          CALL JENONU(JEXNOM('&CATA.TM.NOMTM','TR3QU9'),ITYMA)
          ZI(JAD-1+COMPT(K)+1) = ITYTE
        ELSE IF (NOMTE(K).EQ.'CFQ4T3') THEN
          CALL JENONU(JEXNOM('&CATA.TE.NOMTE','CFQ9Q9'),ITYTE)
          CALL JENONU(JEXNOM('&CATA.TM.NOMTM','QU9QU9'),ITYMA)
          ZI(JAD-1+COMPT(K)+1) = ITYTE
        ELSE
          CALL JXABOR()
        END IF

        JCO = 0
        DO 80,IPC = 1,NBPC
          IF (ZI(JTYNMA-1+2* (IPC-1)+1).NE.ITYMA) GO TO 80
          JCO = JCO + 1
          ZI(JAD-1+JCO) = -IPC
   80   CONTINUE

   90 CONTINUE


C      3. CREATION DE LA CARTE :
C      -------------------------

      CALL ALCART('V',CARTE,NOMA,'NEUT_R',NBPC,NBPC)
      CALL JEVEUO(CARTE//'.NCMP','E',JNCMP)
      CALL JEVEUO(CARTE//'.VALV','E',JVALV)

      NCMP = 32
      DO 100,K = 1,NCMP
        CALL CODENT(K,'G',CH2)
        ZK8(JNCMP-1+K) = 'X'//CH2
  100 CONTINUE

      DO 110,IPC = 1,NBPC
        ZR(JVALV-1+1) = ZR(JTABF+21* (IPC-1)+3)
        ZR(JVALV-1+2) = ZR(JTABF+21* (IPC-1)+4)
        ZR(JVALV-1+3) = ZR(JTABF+21* (IPC-1)+5)
        ZR(JVALV-1+4) = ZR(JTABF+21* (IPC-1)+6)
        ZR(JVALV-1+5) = ZR(JTABF+21* (IPC-1)+7)
        ZR(JVALV-1+6) = ZR(JTABF+21* (IPC-1)+8)
        ZR(JVALV-1+7) = ZR(JTABF+21* (IPC-1)+9)
        ZR(JVALV-1+8) = ZR(JTABF+21* (IPC-1)+10)
        ZR(JVALV-1+9) = ZR(JTABF+21* (IPC-1)+11)
        ZR(JVALV-1+10) = ZR(JTABF+21* (IPC-1)+12)
        ZR(JVALV-1+11) = ZR(JTABF+21* (IPC-1)+13)
        ZR(JVALV-1+12) = ZR(JTABF+21* (IPC-1)+14)
        IZONE = NINT(ZR(JTABF+21* (IPC-1)+15))
        ZR(JVALV-1+13) = ZR(JCMCF+10* (IZONE-1)+2)
        ZR(JVALV-1+14) = ZR(JCMCF+10* (IZONE-1)+3)
        ZR(JVALV-1+15) = ZR(JCMCF+10* (IZONE-1)+4)
        ZR(JVALV-1+16) = NINT(ZR(JCMCF+10* (IZONE-1)+5))
        ZR(JVALV-1+18) = ZI(JECPD+6* (IZONE-1)+1)
        ZR(JVALV-1+19) = ZR(JTABF+21* (IPC-1)+16)
        ZR(JVALV-1+20) = INST(2)        
        ZR(JVALV-1+21) = ZI(JECPD+6* (IZONE-1)+6)
        ZR(JVALV-1+22) = ZR(JTABF+21*(IPC-1)+17)
        ZR(JVALV-1+23) = ZR(JTABF+21*(IPC-1)+18)
        ZR(JVALV-1+24) = ZR(JTABF+21*(IPC-1)+19)
        ZR(JVALV-1+25) = ZR(JTABF+21*(IPC-1)+20)
        ZR(JVALV-1+26) = ZR(JTABF+21* (IPC-1)+21)
        ZR(JVALV-1+27) = NINT(ZR(JCMCF+10* (IZONE-1)+7))
        ZR(JVALV-1+28) = ZR(JCMCF+10* (IZONE-1)+8)
        ZR(JVALV-1+29) = ZR(JCMCF+10* (IZONE-1)+9)
        ZR(JVALV-1+30) = ZR(JCMCF+10* (IZONE-1)+10) 
        ZR(JVALV-1+31) = INST(4) 
        ZR(JVALV-1+32) = INST(5) 
            
        CALL NOCART(CARTE,-3,KBID,'NUM',1,KBID,-IPC,LIGREL,NCMP)

  110 CONTINUE


C      4. MENAGE :
C      -------------------------
      CALL JEDETR(CARTE//'.NCMP')
      CALL JEDETR(CARTE//'.VALV')
      CALL JEDETR('&&FRAPP2.TYPNEMA')

      CALL JEDEMA()
      END
