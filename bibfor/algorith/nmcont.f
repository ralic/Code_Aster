      SUBROUTINE NMCONT(FONACT,SDIMPR,SDERRO,DEFICO,RESOCO,
     &                  MAXREL,SDTIME,CONVER)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/07/2011   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT     NONE
      CHARACTER*24 SDTIME,SDIMPR,SDERRO,DEFICO,RESOCO
      INTEGER      FONACT(*)
      LOGICAL      MAXREL,CONVER

C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (ALGORITHME)
C
C AFFICHAGE DU TABLEAU DE CONVERGENCE
C
C ----------------------------------------------------------------------
C
C

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
      CHARACTER*16 K16BLA
      LOGICAL      LTABL,ERROR
      REAL*8       TIME
      INTEGER      IBID,MMITGO
      LOGICAL      ISFONC,LCTCD
      LOGICAL      CFDISL,LALLV,CTCGEO,CTCFIX
      CHARACTER*24 CLREAC
      INTEGER      JCLREA
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- FONCTIONNALITES ACTIVEES
C
      LCTCD  = ISFONC(FONACT,'CONT_DISCRET')
C
C --- ERREUR OU PAS ?
C
      CALL NMERGE(SDERRO,'GET','ALL',ERROR )
C
C --- INITIALISATIONS
C
      K16BLA = ' ' 
C
C --- TEMPS PASSE DANS L'ITERATION
C      
      CALL NMTIMR(SDTIME,'TEMPS_PHASE','N',TIME)
      CALL IMPSDR(SDIMPR,'ITER_TIME',K16BLA,TIME  ,IBID  )
C
C --- AFFICHAGE LIGNE DU TABLEAU DE CONVERGENCE + RESUME
C
      CALL NMITAB(FONACT,DEFICO,CONVER,ERROR ,LTABL  )
      IF (LTABL) THEN
        IF (CONVER) THEN
          IF (MAXREL) THEN
            CALL NMCVGI(SDIMPR,'CVG_MX')
          ELSE
            CALL NMCVGI(SDIMPR,'CVG_OK')
          ENDIF
        ELSE
          CALL NMIMPR(SDIMPR,'TABL',' ',K16BLA,0.D0,0)
        ENDIF
      ENDIF
C
C --- CONTACT DISCRET SANS BOUCLE GEOMETRIQUE: AFFICHAGE
C
      IF (LCTCD) THEN
        LALLV  = CFDISL(DEFICO,'ALL_VERIF')
        IF (.NOT.LALLV) THEN
          CLREAC = RESOCO(1:14)//'.REAL'
          CALL JEVEUO(CLREAC,'L',JCLREA)
          CTCGEO = .NOT.ZL(JCLREA+1-1) 
          CTCFIX = ZL(JCLREA+2-1)
          IF ((.NOT.CTCFIX).AND.
     &        (.NOT.CTCGEO)) THEN
            CALL MMBOUC(RESOCO,'GEOM','INCR',MMITGO)
            CALL NMIMPR(SDIMPR,'IMPR','BCL_GEOME',' ',0.D0,MMITGO)
            CALL NMIMPR(SDIMPR,'IMPR','LIGNE',' ',0.D0,0)
          ENDIF
        ENDIF
      ENDIF
C
      CALL JEDEMA()
      END
