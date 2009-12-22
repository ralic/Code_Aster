      SUBROUTINE CFINIG(RESOCO,ITERAT,LNOPRE)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 22/12/2009   AUTEUR ABBAS M.ABBAS 
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
      INTEGER      ITERAT
      CHARACTER*24 RESOCO
      LOGICAL      LNOPRE
C      
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (INITIALISATION CONTACT DISCRET)
C
C INITIALISATION DES PARAMETRES DE CONTACT POUR LA BOUCLE
C DE POINT FIXE DE LA GEOMETRIE
C
C ----------------------------------------------------------------------
C
C
C IN  RESOCO : SD RESOLUTION DU CONTACT
C IN  ITERAT : NUMERO D'ITERATION DE NEWTON
C OUT LNOPRE : .TRUE. SI ON DOIT SAUTER LA PREDICTION
C
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
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
C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------
C
      CHARACTER*24 CLREAC
      INTEGER      JCLREA 
      LOGICAL      REAGEO,REAPRE
      INTEGER      MMITGO  
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ() 
C
C --- ACCES OBJETS
C
      CLREAC = RESOCO(1:14)//'.REAL'             
      CALL JEVEUO(CLREAC,'E',JCLREA)  
C
C --- DOIT-ON FAIRE UNE REAC. GEOM ?      
C             
      IF (ITERAT.EQ.0) THEN         
        REAGEO = .TRUE.
      ELSE
        REAGEO = .FALSE.  
      ENDIF
C
C --- EST-CE LA PREMIERE REAC. GEOM ?
C      
      CALL MMBOUC(RESOCO,'GEOM','READ',MMITGO) 
      IF ((MMITGO.EQ.1).AND.(ITERAT.EQ.0)) THEN
        REAPRE = .TRUE.
      ELSE
        REAPRE = .FALSE.  
      ENDIF      
C
C --- DOIT-ON PASSER EN PREDICTION ?
C      
      IF (MMITGO.EQ.2) THEN
        LNOPRE = .TRUE.
      ELSE
        LNOPRE = .FALSE.  
      ENDIF      
C
      ZL(JCLREA+1-1) = REAGEO   
      ZL(JCLREA+3-1) = REAPRE
C 
      CALL JEDEMA()

      END
