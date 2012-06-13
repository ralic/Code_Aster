      SUBROUTINE RC3600
      IMPLICIT   NONE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ------------------------------------------------------------------
C
C     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3600
C
C     LA PREMIERE ETAPE EST DE TRADUIRE LES DONNEES EN CHAM_ELEM_S
C     PUIS DE CALCULER LES SP, SN, ... EN CHAQUE NOEUD DE CHAQUE MAILLE
C     LE RESULTAT EST UN CHAM_ELEM_S QUE L'ON TRADUIRA DANS UNE TABLE
C
C     ------------------------------------------------------------------
C
      INCLUDE 'jeveux.h'
      INTEGER      N1, NBTOU, IERD, NBMA, JMA, IMA, NBCMP, NBMAT, IBID,
     +             IFM, NIV
      CHARACTER*8  K8B, NOMRES, NOMA, CARAEL, MODELE, NOMMAT,
     +             MOTCLS(2), TYPMCS(2), NOMGD
      CHARACTER*16 NOMCMD, CONCEP, MOTCLF, NOCMP(5)
      CHARACTER*24 MESMAI, NCNCIN, CHINDI, CHCARA, CHRESU
      INTEGER      IARG
C DEB ------------------------------------------------------------------
      CALL JEMARQ()
C
      CALL INFNIV ( IFM, NIV )
C
      CALL GETRES ( NOMRES , CONCEP , NOMCMD )
C
C     ------------------------------------------------------------------
C               LE MATERIAU , MODELE , CARA_ELEM
C     ------------------------------------------------------------------
      CALL GETVID ( ' ', 'CHAM_MATER', 1,IARG,1, NOMMAT, N1 )
      CALL GETVID ( ' ', 'MODELE'    , 1,IARG,1, MODELE, N1 )
      CALL GETVID ( ' ', 'CARA_ELEM' , 1,IARG,1, CARAEL, N1 )
C
      CALL DISMOI('F','NOM_MAILLA',MODELE,'MODELE',IBID,NOMA,IERD)
      CALL DISMOI('F','NB_MA_MAILLA',NOMA,'MAILLAGE',NBMAT,K8B,IERD)
C
C     ------------------------------------------------------------------
C                           ZONE D'ANALYSE
C     ------------------------------------------------------------------
C
      MOTCLF = 'ZONE_ANALYSE'
C
      MESMAI = '&&RC3600.MES_MAILLES'
      MOTCLS(1) = 'GROUP_MA'
      MOTCLS(2) = 'MAILLE'
      TYPMCS(1) = 'GROUP_MA'
      TYPMCS(2) = 'MAILLE'      
C
      CALL GETVTX ( MOTCLF, 'TOUT', 1,IARG, 1, K8B, NBTOU )
      IF ( NBTOU .NE. 0 ) THEN
         NBMA = NBMAT
         CALL WKVECT ( MESMAI, 'V V I' , NBMA, JMA )
         DO 10 IMA = 1 , NBMA
            ZI(JMA+IMA-1) = IMA
 10      CONTINUE
      ELSE
         CALL RELIEM ( ' ', NOMA, 'NU_MAILLE', MOTCLF, 1, 2, 
     +                                  MOTCLS, TYPMCS, MESMAI, NBMA )
         CALL JEVEUO ( MESMAI, 'L', JMA )
      ENDIF
C
      NCNCIN = '&&RC3600.CONNECINVERSE  '
      CALL JEEXIN ( NCNCIN, IBID )
      IF (IBID.EQ.0) CALL CNCINV ( NOMA, IBID, 0, 'V', NCNCIN )
C
C     ------------------------------------------------------------------
C              RECUPERATION DES CARACTERISTIQUES MATERIAU
C     ------------------------------------------------------------------
C
      CALL RC36MA ( NOMMAT, NOMA )
C
C     ------------------------------------------------------------------
C            DEFINITION DES CARACTERISTIQUES ELEMENTAIRES
C     ------------------------------------------------------------------
C
      CHCARA = '&&RC3600.CARA_ELEM'
C
      CALL RC36CA ( CARAEL, NOMA, NBMA, ZI(JMA), CHCARA )
C
      IF ( NIV .GE. 2 ) THEN
         WRITE(IFM,*)' LE CHAMP ', CHCARA
         CALL CESIMP ( CHCARA, IFM, 0, IBID )
      ENDIF
C
C
C     ------------------------------------------------------------------
C                    LES INDICES DE CONTRAINTES
C     ------------------------------------------------------------------
C
      CHINDI = '&&RC3600.INDI_SIGM'
C
      CALL RC36IN ( NOMA, NBMA, ZI(JMA), CHINDI )
C
      IF ( NIV .GE. 2 ) THEN
         WRITE(IFM,*)' LE CHAMP ', CHINDI
         CALL CESIMP ( CHINDI, IFM, 0, IBID )
      ENDIF
C
C     ------------------------------------------------------------------
C                 LES RESULTATS DES CALCULS MECANIQUES
C     ------------------------------------------------------------------
C
      CALL RC36RM
C
C     ------------------------------------------------------------------
C                           LES SITUATIONS
C     ------------------------------------------------------------------
C
      CALL RC36SI ( NOMA, NBMA, ZI(JMA) )
C
C     ------------------------------------------------------------------
C              CALCULS DES AMPLITUDES DE CONTRAINTES
C     ------------------------------------------------------------------
C
C     CALCUL DES AMPLITUDES DE CONTRAINTES QUI CORRESPONDENT AUX
C     COMBINAISONS DE TOUS LES ETATS STABILISES APPARTENANT AUX
C     SITUATIONS D'UN GROUPE DONNE
C
      NOMGD = 'RCCM_R'
      NBCMP = 5
      NOCMP(1) = 'SM'
      NOCMP(2) = 'SN'
      NOCMP(3) = 'SN_3SM'
      NOCMP(4) = 'SALT'
      NOCMP(5) = 'U_TOTAL'
C
      CHRESU = 'RC3600.RESULTAT'
      CALL RC36ZZ ( NOMA, NOMGD, NBCMP, NOCMP, NBMA, ZI(JMA), CHRESU )
C
C --- CALCUL DES AMPLITUDES DE CONTRAINTES
C     CALCUL DU FACTEUR D'USAGE 
C     -------------------------
C
      CALL RC36AC ( NOMA, NCNCIN, CHINDI, CHCARA, 
     +                              NBMA, ZI(JMA), CHRESU )
C
      IF ( NIV .GE. 2 ) THEN
         WRITE(IFM,*)' LE CHAMP ', CHRESU
         CALL CESIMP ( CHRESU, IFM, 0, IBID )
      ENDIF
C
C
C --- PASSAGE DU CHAM_ELEM A UNE TABLE
C     --------------------------------
C
      CALL RC36RS ( NOMRES, NOMA, NBMA, ZI(JMA), CHINDI, CHRESU )
C
      CALL DETRSD ( 'CHAM_ELEM_S', CHINDI )
      CALL DETRSD ( 'CHAM_ELEM_S', CHCARA )
      CALL DETRSD ( 'CHAM_ELEM_S', CHRESU )
      CALL JEEXIN ( NCNCIN, IBID )
      IF (IBID .NE. 0)  CALL JEDETR ( NCNCIN )
      CALL JEDETC ( 'V' , '&&RC3600' , 1 )
C
      CALL JEDEMA( )
      END
