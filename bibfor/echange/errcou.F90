subroutine errcou(nomprg, numpas, nomvar, info, nprog,&
                  nlu)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: nicolas.greffet at edf.fr
! **********************************************************************
! *   LOGICIEL CODE_ASTER - COUPLAGE ASTER/EDYOS - COPYRIGHT EDF 2009  *
! **********************************************************************
!
!PAT_FONCTION
!
!  ERRCOU : FONCTION
!
!    CE SSP PERMET DE VERIFIER LA BONNE EXECUTION DES APPELS YACS
!    EN CAS D'ERREUR, IL INTERPRETE LE CODE ERREUR FOURNI PAR YACS ET
!    L'ECRIT SUR LES FICHIERS DE SORTIE.
!    AU 13/02/09 : MESSAGE D'ERREUR CALCIUM
!
!    EN PHASE DE DEBUGGAGE :
!    ERRCOU VERIFIE EGALEMENT QUE LA DIMENSION DE LA VARIABLE TEL QUE
!    TRAITEE PAR YACS (NLU) CORRESPOND BIEN A CELLE QUI ETAIT PREVUE
!    ET PROGRAMMEE (NPROG) CE TEST A UNIQUEMENT UN SENS POUR LES APPELS
!    EN LECTURE DE YACS. POUR LES APPELS EN ECRITURE NPROG=NLU
!    (FIXE DANS LE PROGRAMME APPELANT).
!
!
!     ERRCOU EST APPELE PAR TOUS LES SSP FAISANT APPEL A YACS ET FAIT
!     APPEL A UN PROGRAMME PYTHON "edyos.py" CONTENANT LES MESSAGES
!     D'ERREURS
!
!PAT_FONCTION
!
!.======================================================================
!
!  REFERENCES BIBLIOGRAPHIQUES
!  ---------------------------
!
!  NOTE HI-26/03/007A
!  "DEVELOPPEMENT D'UN MODE PRODUCTION POUR CALCIUM: MANUEL UTILISATEUR"
!  ANNEXE 1: CODES D'ERREURS  (PAGE 70)
!  FAYOLLE ERIC, DEMKO BERTRAND (CS SI)  JUILLET 2003
!
! ======================================================================
!
!
!  DEVELOPPEMENTS ET CORRECTIONS D'ANOMALIES
!  -----------------------------------------
!
!  DATE: 13/02/09   AUTEUR: P. VAUGRANTE    ANOMALIE: DEVELOPPEMENT
!  DATE:            AUTEUR:                 ANOMALIE:
!  DATE:            AUTEUR:                 ANOMALIE:
!  DATE:            AUTEUR:                 ANOMALIE:
!
!
! ======================================================================
!  VARIABLES UTILISEES
!  -------------------
!
!  ____________________________________________________________________
! !    NOM  !   TYPE      !                  ROLE                      !
! !_________!_____________!____________________________________________!
! !         !             !                                            !
! !         !             !                                            !
! ! NUMPAS  !  ENTIER     !  NUMERO D'ITERATION PENDANT LEQUEL IL Y A  !
! !         !             !  EU UN PROBLEME                            !
! !         !             !                                            !
! ! NOMPRG  !  CHARACTER  !  NOM DU SSP DANS LEQUEL IL Y A EU PROBLEME !
! !         !             !                                            !
! ! NOMVAR  !  CHARACTER  !  NOM DE LA VARIABLE POSANT PROBLEME        !
! !         !             !                                            !
! !         !             !                                            !
! ! INFO    !  ENTIER     !  FLAG D'EXECUTION RETOUNE PAR YACS         !
! !         !             !                                            !
! ! NPROG   !  ENTIER     !  DIMENSION DE LA VARIABLE ECHANGEE         !
! !         !             !  (TELLE QUE PROGRAMMEE)                    !
! !         !             !                                            !
! ! NLU     !  ENTIER     !  DIMENSION DE LA VARIABLE ECHANGEE         !
! !         !             !  (RENVOYE PAR YACS)                        !
! !         !             !                                            !
! !_________!_____________!____________________________________________!
!
!
!
!
!
!=======================================================================
!  SOUS PROGRAMME(S) APPELE(S) : AUCUN
!
!-----------------------------------------------------------------------
!  SOUS PROGRAMME(S) APPELANT(S) :  INIPAT.F, CHAPAT.F, ECRPAT.F
!
!***********************************************************************
!%W% %D% %T%
!
! aslint: disable=W1304
    implicit none
!
!     ARGUMENTS
!     ==============
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/utmess.h"
    character(len=8) :: nomprg
    integer(kind=4) :: numpas, info, nprog, nlu
    integer, parameter :: lenvar=144
    character(len=lenvar) :: nomvar
!
!
!
!
!
!     VARIABLES INTERNES
!     ==================
!
    character(len=lenvar) :: valk(2)
    integer :: vali(1)
!
    call jemarq()
!     PAS DE PROBLEME YACS
!     ====================
    if (info .eq. 0 .and. nprog .eq. nlu) goto 9999
    if (info .eq. 0 .and. nprog .ne. nlu) then
        valk(1)=nomprg
        valk(2)=nomvar
        call utmess('A', 'EDYOS_41', nk=2, valk=valk)
        goto 9999
    endif
!
!     PROBLEMES LORS DE L'APPEL YACS
!     ==============================
    valk(1)=nomprg
    valk(2)=nomvar
    vali(1)=numpas
    call utmess('F', 'EDYOS_42', nk=2, valk=valk, si=vali(1))
    if (info .eq. 1) then
        call utmess('F', 'EDYOS_1')
    endif
!
    if (info .eq. 2) then
        call utmess('F', 'EDYOS_2')
    endif
!
    if (info .eq. 3) then
        call utmess('F', 'EDYOS_3')
    endif
!
    if (info .eq. 4) then
        call utmess('F', 'EDYOS_4')
    endif
!
    if (info .eq. 5) then
        call utmess('F', 'EDYOS_5')
    endif
!
    if (info .eq. 6) then
        call utmess('F', 'EDYOS_6')
    endif
!
    if (info .eq. 7) then
        call utmess('F', 'EDYOS_7')
    endif
!
    if (info .eq. 8) then
        call utmess('F', 'EDYOS_8')
    endif
!
    if (info .eq. 9) then
        call utmess('F', 'EDYOS_9')
    endif
!
    if (info .eq. 10) then
        call utmess('F', 'EDYOS_10')
    endif
!
    if (info .eq. 11) then
        call utmess('F', 'EDYOS_11')
    endif
!
    if (info .eq. 12) then
        call utmess('F', 'EDYOS_12')
    endif
!
    if (info .eq. 13) then
        call utmess('F', 'EDYOS_13')
    endif
!
    if (info .eq. 14) then
        call utmess('F', 'EDYOS_14')
    endif
!
    if (info .eq. 15) then
        call utmess('F', 'EDYOS_15')
    endif
!
    if (info .eq. 16) then
        call utmess('F', 'EDYOS_16')
    endif
!
    if (info .eq. 17) then
        call utmess('F', 'EDYOS_17')
    endif
!
    if (info .eq. 18) then
        call utmess('F', 'EDYOS_18')
    endif
!
    if (info .eq. 19) then
        call utmess('F', 'EDYOS_19')
    endif
!
    if (info .eq. 20) then
        call utmess('F', 'EDYOS_20')
    endif
!
    if (info .eq. 21) then
        call utmess('F', 'EDYOS_21')
    endif
!
    if (info .eq. 22) then
        call utmess('F', 'EDYOS_22')
    endif
!
    if (info .eq. 23) then
        call utmess('F', 'EDYOS_23')
    endif
!
    if (info .eq. 24) then
        call utmess('F', 'EDYOS_24')
    endif
!
    if (info .eq. 25) then
        call utmess('F', 'EDYOS_25')
    endif
!
    if (info .eq. 26) then
        call utmess('F', 'EDYOS_26')
    endif
!
    if (info .eq. 27) then
        call utmess('F', 'EDYOS_27')
    endif
!
    if (info .eq. 28) then
        call utmess('F', 'EDYOS_28')
    endif
!
    if (info .eq. 29) then
        call utmess('F', 'EDYOS_29')
    endif
!
    if (info .eq. 30) then
        call utmess('F', 'EDYOS_30')
    endif
!
    if (info .eq. 31) then
        call utmess('F', 'EDYOS_31')
    endif
!
    if (info .eq. 32) then
        call utmess('F', 'EDYOS_32')
    endif
!
    if (info .eq. 33) then
        call utmess('F', 'EDYOS_33')
    endif
!
    if (info .eq. 34) then
        call utmess('F', 'EDYOS_34')
    endif
!
    if (info .eq. 35) then
        call utmess('F', 'EDYOS_35')
    endif
!
    if (info .eq. 36) then
        call utmess('F', 'EDYOS_36')
    endif
!
    if (info .eq. 37) then
        call utmess('F', 'EDYOS_37')
    endif
!
    if (info .eq. 38) then
        call utmess('F', 'EDYOS_38')
    endif
!
    if (info .eq. 39) then
        call utmess('F', 'EDYOS_39')
    endif
!
    if (info .eq. 40) then
        call utmess('F', 'EDYOS_40')
    endif
!
!     SORTIE DE L'EXECUTION
!     =====================
!
9999  continue
    call jedema()
!
end subroutine
