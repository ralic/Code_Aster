      SUBROUTINE NMCORT(SDCRIT,NRESI ,IRESI ,VRESI ,VRESID,
     &                  PLATIT,PLATRE,CONVOK)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/09/2008   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
      CHARACTER*19 SDCRIT
      INTEGER      IRESI,NRESI
      REAL*8       VRESI,VRESID            
      LOGICAL      CONVOK
      INTEGER      PLATIT
      REAL*8       PLATRE
C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (ALGORITHME - CONVERGENCE)
C
C VERIFICATION DES CRITERES D'ARRET SUR RESIDU - OPTION PLATEAU
C
C ----------------------------------------------------------------------
C
C
C IN  SDCRIT : SYNTHESE DES RESULTATS DE CONVERGENCE POUR ARCHIVAGE
C IN  NRESI  : NOMBRE DE RESIDUS A EVALUER
C IN  IRESI  : NUMERO DU RESIDU A TESTER
C IN  VRESI  : NORME MAXI DU RESIDU A EVALUER
C IN  VRESID : DONNEE UTILISATEUR POUR CONVERGENCE
C IN  PLATIT : LONGUEUR MAXI DU PLATEAU
C IN  PLATRE : LARGEUR DU TUNNEL AUTOUR DU PLATEAU
C OUT CONVOK . .TRUE. SI CRITERE RESPECTE
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
      CHARACTER*24  CRITPL
      INTEGER       JCRP
      REAL*8        OLDMIN,OLDMAX
      REAL*8        PLATHE,R8MAEM
      INTEGER       NBRITE
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- ACCES OBJETS JEVEUX
C
      CRITPL = SDCRIT(1:19)//'.PLAT'
      CALL JEVEUO(CRITPL,'E',JCRP  )
C
C --- INITIALISATIONS
C      
      CONVOK = .TRUE.
      OLDMAX = ZR(JCRP+4*(IRESI-1)+2-1)
      OLDMIN = ZR(JCRP+4*(IRESI-1)+1-1)
C
C --- NOUVEAUX MIN/MAX
C
      IF (VRESI.LE.OLDMIN) THEN
        ZR(JCRP+4*(IRESI-1)+1-1) = VRESI
      ENDIF      
      IF (VRESI.GE.OLDMAX) THEN
        ZR(JCRP+4*(IRESI-1)+2-1) = VRESI
      ENDIF
C
C --- NOUVELLE ITERATION
C
      ZR(JCRP+4*(IRESI-1)+4-1) = ZR(JCRP+4*(IRESI-1)+4-1)+1.D0    
C
C --- VALEUR DU "TUNNEL" DU PLATEAU
C          
      PLATHE = ABS(ZR(JCRP+4*(IRESI-1)+2-1) -
     &             ZR(JCRP+4*(IRESI-1)+1-1))
C
C --- UNE ITERATION DE PLUS DANS LE TUNNEL ?
C          
      IF (PLATHE.LE.PLATRE) THEN
        ZR(JCRP+4*(IRESI-1)+3-1) = ZR(JCRP+4*(IRESI-1)+3-1)+1.D0
      ELSE
        ZR(JCRP+4*(IRESI-1)+3-1) = 0.D0   
      ENDIF   
      NBRITE = NINT(ZR(JCRP+4*(IRESI-1)+3-1))
C
C --- PLATEAU ASSEZ LONG ?
C          
      IF (NBRITE.EQ.PLATIT) THEN 
        CALL NMCORU(VRESI ,VRESID,CONVOK)
        ZR(JCRP+4*(IRESI-1)+1-1) = R8MAEM()
        ZR(JCRP+4*(IRESI-1)+2-1) = -R8MAEM() 
        ZR(JCRP+4*(IRESI-1)+3-1) = 0.D0   
      ELSE
        CONVOK = .FALSE.  
      ENDIF
C
C --- REMISE A ZERO DU PLATEAU
C
      IF (NINT(ZR(JCRP+4*(IRESI-1)+4-1)).EQ.PLATIT) THEN
        ZR(JCRP+4*(IRESI-1)+1-1) = R8MAEM()
        ZR(JCRP+4*(IRESI-1)+2-1) = -R8MAEM() 
        ZR(JCRP+4*(IRESI-1)+3-1) = 0.D0 
        ZR(JCRP+4*(IRESI-1)+4-1) = 0.D0      
      ENDIF
C
      CALL JEDEMA()    
      END
