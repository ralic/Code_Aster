      SUBROUTINE IBOPMC(NOM, DESCR, PTR1, PTR2, PTR3, IER)
      IMPLICIT NONE
      CHARACTER*(*)     NOM(*),DESCR(*)
      INTEGER           PTR1(*),PTR2(*),PTR3(*)
      INTEGER           IER
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 15/10/96   AUTEUR GIBHHCM C.MASSERET 
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
C     ------------------------------------------------------------------
C     NOM(*)   IN : MOTS CLE DE LA COMMANDE
C     DESCR(*) IN : DESCRIPTEUR DU MOT CLE
C     PTR1(*)  IN : POINTEURS : NB DE MOTS CLE DANS LE MCF 
C                               POSITION VALEUR PAR DEFAUT POUR UN MC
C     PTR2(*)  IN : POINTEURS : POSITION DEBUT VALEURS PERMISES (MC) 
C     PTR3(*)  IN : POINTEURS : POSITION FIN   VALEURS PERMISES (MC) 
C     IER      IN/OUT : +1 SI INCOHERENCE DETECTEE
C     ------------------------------------------------------------------
C     VERIFICATION QUE LE STATUT 'CAHCHE' D'UN MOT CLE SIMPLE
C     CORRESPOND BIEN A SA DEFINITION :
C         - UNE SEULE VALEUR PERMISE
C         - UNE VALEUR PAR DEFAUT
C     LORS DE LA COMPILATION DES COMMANDES.
C     ------------------------------------------------------------------
C
      INTEGER II
C            
      II=2
C     --- BOUCLE SUR TOUS LES MOTS CLE DE LA COMMANDE ---
 101  CONTINUE
      IF (II .LE. PTR1(1)) THEN
         IF (DESCR(II)(1:1) .NE. '*') THEN
C           --- MOT CLE SIMPLE ---
            IF (DESCR(II)(3:3) .EQ. 'C') THEN
C              MOT CLE SIMPLE CACHE
               IF (PTR1(II) .EQ. 0) THEN
                   CALL UTDEBM('E','COMPILATION DES COMMANDES',
     +                 'DEFINITION INTERDITE D''MOT CLE CACHE')
                   CALL UTIMPK('L','LE MOT CLE :',1,NOM(II))
                   CALL UTIMPK('L','DE LA COMMANDE :',1,NOM)
                   CALL UTIMPK('L','N''A PAS DE VALEUR PAR DEFAUT',
     +                                0,' ')
                    CALL UTFINM()
                    IER=IER+1
               ENDIF
               IF (PTR2(II) .EQ. 0) THEN
                   CALL UTDEBM('E','COMPILATION DES COMMANDES',
     +                 'DEFINITION INTERDITE D''MOT CLE CACHE')
                   CALL UTIMPK('L','LE MOT CLE :',1,NOM(II))
                   CALL UTIMPK('L','DE LA COMMANDE :',1,NOM)
                   CALL UTIMPK('L','N''A PAS DE VALEUR PERMISE',
     +                                0,' ')
                    CALL UTFINM()
                  IER=IER+1
               ELSE IF (PTR2(II) .NE. PTR3(II)) THEN
                   CALL UTDEBM('E','COMPILATION DES COMMANDES',
     +                 'DEFINITION INTERDITE D''MOT CLE CACHE')
                   CALL UTIMPK('L','LE MOT CLE :',1,NOM(II))
                   CALL UTIMPK('L','DE LA COMMANDE :',1,NOM)
                   CALL UTIMPK('L','A PLUS D''UNE VALEUR PERMISE',
     +                                0,' ')
                    CALL UTFINM()
                  IER=IER+1
               ENDIF
            ENDIF
            II=II+1
         ELSE
C           --- MCF ON N'ANALYSE QUE SES MC SIMPLES ---
            II=II+1
         ENDIF
         GOTO 101
      ENDIF  
C     
      END
