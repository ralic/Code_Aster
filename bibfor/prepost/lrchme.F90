subroutine lrchme(chanom, nochmd, nomamd, nomaas, typech,&
                  nomgd, typent, nbcmpv, ncmpva, ncmpvm,&
                  prolz, iinst, numpt, numord, inst,&
                  crit, prec, nrofic, option, param,&
                  nbpgma, nbpgmm, codret)
!
! person_in_charge: nicolas.sellenet at edf.fr
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     LECTURE D'UN CHAMP NOEUD/ELEMENT - FORMAT MED
!     -    -       --                           --
!-----------------------------------------------------------------------
!     ENTREES:
!        CHANOM  : NOM ASTER DU CHAMP A LIRE
!        NOCHMD : NOM MED DU CHAMP DANS LE FICHIER
!        NOMAMD : NOM MED DU MAILLAGE LIE AU CHAMP A LIRE
!                  SI ' ' : ON SUPPOSE QUE C'EST LE PREMIER MAILLAGE
!                           DU FICHIER
!        NOMAAS : NOM ASTER DU MAILLAGE
!        TYPECH : TYPE DU CHAMP
!        TYPENT : TYPE D'ENTITE DU CHAMP
!                (MED_NOEUD=3,MED_MAILLE=0,MED_NOEUD_MAILLE=4)
!        NOMGD  : NOM DE LA GRANDEUR ASSOCIEE AU CHAMP
!        NBCMPV : NOMBRE DE COMPOSANTES VOULUES
!                 SI NUL, ON LIT LES COMPOSANTES A NOM IDENTIQUE
!        NCMPVA : LISTE DES COMPOSANTES VOULUES POUR ASTER
!        NCMPVM : LISTE DES COMPOSANTES VOULUES DANS MED
!        PROLZ  : VALEUR DE PROL_ZERO ('OUI' OU 'NAN')
!        IINST  : 1 SI LA DEMANDE EST FAITE SUR UN INSTANT, 0 SINON
!        NUMPT  : NUMERO DE PAS DE TEMPS EVENTUEL
!        NUMORD : NUMERO D'ORDRE EVENTUEL DU CHAMP
!        INST   : INSTANT EVENTUEL
!        CRIT   : CRITERE SUR LA RECHERCHE DU BON INSTANT
!        PREC   : PRECISION SUR LA RECHERCHE DU BON INSTANT
!        NROFIC : NUMERO NROFIC LOGIQUE DU FICHIER MED
!        OPTION / PARAM : POUR CREER LE CHAMP COMME S'IL ETAIT LE
!                 PARAMETRE EN SORTIE DE CETTE OPTION (CHAMPS ELGA)
!     SORTIES:
!        CODRET : CODE DE RETOUR (0 : PAS DE PB, NON NUL SI PB)
!_____________________________________________________________________
!
! aslint: disable=W1504
    implicit none
!
! 0.1. ==> ARGUMENTS
!
#include "asterc/getvid.h"
#include "asterfort/lrceme.h"
#include "asterfort/lrcnme.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
    character(len=19) :: chanom
    character(len=*) :: ncmpva, ncmpvm
    character(len=8) :: nomaas
    character(len=8) :: nomgd, typech
    character(len=3) :: prolz
    character(len=8) :: crit, param
    character(len=24) :: option
    character(len=64) :: nochmd, nomamd
!
    integer :: nrofic, typent
    integer :: codret
    integer :: nbcmpv
    integer :: iinst, numpt, numord
    integer :: nbpgma(*), nbpgmm(*)
!
    real(kind=8) :: inst
    real(kind=8) :: prec
!
! 0.2. ==> COMMUNS
!
! 0.3. ==> VARIABLES LOCALES
!
!
    character(len=8) :: nommod
    integer :: iaux
    integer :: iarg
!
!====
! 1. LECTURE DANS LE FICHIER MED
!====
!
!
    if (typech(1:2) .eq. 'NO') then
        call lrcnme(chanom, nochmd, nomamd, nomaas, nomgd,&
                    typent, nbcmpv, ncmpva, ncmpvm, iinst,&
                    numpt, numord, inst, crit, prec,&
                    nrofic, codret)
    else if (typech(1:2).eq.'EL'.or.typech(1:2).eq.'CA') then
        call getvid(' ', 'MODELE', 0, iarg, 1,&
                    nommod, iaux)
        if (iaux .eq. 0 .and. typech(1:4) .ne. 'CART') then
            call u2mess('F', 'MED_71')
        endif
        if (iaux .eq. 0) nommod = ' '
        call lrceme(chanom, nochmd, typech(1:4), nomamd, nomaas,&
                    nommod, nomgd, typent, nbcmpv, ncmpva,&
                    ncmpvm, prolz, iinst, numpt, numord,&
                    inst, crit, prec, nrofic, option,&
                    param, nbpgma, nbpgmm, codret)
    else
        codret = 1
        call u2mesk('A', 'MED_92', 1, typech(1:4))
    endif
!
!====
! 2. BILAN
!====
!
    if (codret .ne. 0) then
        call u2mesk('A', 'MED_55', 1, chanom)
    endif
!
end subroutine
