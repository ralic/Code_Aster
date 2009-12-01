      SUBROUTINE GNOMS2 ( NOOJB,K1,K2 )
      IMPLICIT   NONE
C     -----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 12/01/2009   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
C RESPONSABLE PELLET J.PELLET
C BUT :
C  TROUVER UN NOM POSSIBLE POUR UN OBJET JEVEUX QUI RESPECTE :
C     - CE NOM VAUT NOOJB (DONNE EN ENTREE) SAUF POUR LA SOUS-CHAINE
C           NOOJB(K1:K2)
C     - LE NOM DE L'OBJET N'EXISTE PAS ENCORE DANS LES BASES OUVERTES
C     - LE NOM (K1:K2) EST UN NUMERO ('0001','0002', ...)

C VAR : NOOJB : NOM D'UN OBJET JEVEUX  (K24)
C IN  : K1,K2 : INDICES DANS NOOJB DE LA SOUS-CHAINE "NUMERO"
C     -----------------------------------------------------------------
C
      INTEGER         INUM,IRET,K1,K2,NESSAI,NDIGIT,IESSAI
      CHARACTER*8     NOMU
      CHARACTER*24    NOOJB,NOOJB1
C     -----------------------------------------------------------------
      CALL ASSERT(K2.GT.K1)
      CALL ASSERT(K1.GT.8)
      CALL ASSERT(K2.LE.24)

      NDIGIT=MIN(K2-K1+1,4)
      NESSAI=INT(10**NDIGIT)

      NOOJB1 = NOOJB
      INUM = -1
      DO 10, IESSAI=1,NESSAI
         INUM = INUM + 1
C        CALL ASSERT(INUM.LE.9998)
         CALL CODENT( INUM , 'D0' , NOOJB1(K1:K2)  )
         CALL JEEXIN ( NOOJB1,IRET )
         IF ( IRET .EQ. 0 ) GO TO 20
 10   CONTINUE
      CALL U2MESS('F','MODELISA4_69')

 20   CONTINUE
      NOOJB=NOOJB1


      END
