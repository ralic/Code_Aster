subroutine convnu(numin, numout, nomvec, base, neqout)
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
!***********************************************************************
!    P. RICHARD     DATE 25/01/92
!-----------------------------------------------------------------------
!  BUT:  <  CONVECRSION DE NUMEROTATION >
!    CREER UN VECTEUR PERMETTANT DE PASSER D'UNE NUMEROTATION
!  A UNE AUTRE, CE VECTEUR DONNE POUR CHAQUEEQUATION DE LA NUMEROTATION
!  RESULTAT LE RANG DE L'EQUTION CORRESPONDANTE DANS LA NUMEROTATION
!   DE DEPART
    implicit none
!
!   SEULS LES DDL PHYSIQUES SONT RESTITUE, DONC SLES LAGRANGES SONT
!   AUTOMATIQUEMENT MIS A ZERO
!
!  CETTE ROUTINE ENVOIE UN MESSAGE D'ALARME SI IL APPARAIT UNE PERTE
!  AU NIVEAU DES DDL PHYSIQUE
!  CE QUI REVIENT A DIRE QUE LA DIFFERENCE DOIT RESIDER
!   DANS LES LAGRANGES
!-----------------------------------------------------------------------
!
! NUMIN    /I/: NOM UT DE LA NUMEROTATION DE DEPART
! NUMOUT   /I/: NOM UT DE LA NUMEROTATION FINALE
! NOMVEC   /I/: NOM K24 DU VECTEUR D'ENTIER RESULTAT
! BASE     /I/: TYPE DE LA BASE JEVEUX 'G' OU 'V'
! NEQOUT   /O/: NOMBRE D'EQUATION DE LA NUMEROTATION RESULTAT
!
!
!
!
#include "jeveux.h"
#include "asterfort/cheddl.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    character(len=1) :: base
    character(len=8) :: maiin, maiout
    character(len=19) :: numin, numout
    character(len=24) :: nomvec
    character(len=24) :: valk(4)
    logical :: erreur
!
    integer :: ibid
    integer :: vali(2)
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, iran(1), ityp, ldcvn
    integer :: neqin, neqout, nuno
    integer, pointer :: nlldein(:) => null()
    integer, pointer :: nlldeou(:) => null()
!-----------------------------------------------------------------------
    data ibid/0/
!-----------------------------------------------------------------------
!
!
!
!--------RECUPERATION DES MAILLAGE ET VERIFICATION COMPATIBILITE--------
!
    call jemarq()
    erreur = .false.
    call dismoi('NOM_MAILLA', numin, 'NUME_DDL', repk=maiin)
    call dismoi('NOM_MAILLA', numout, 'NUME_DDL', repk=maiout)
!
    if (maiin .ne. maiout) then
        valk (1) = numin
        valk (2) = maiin
        valk (3) = numout
        valk (4) = maiout
        call utmess('F', 'ALGORITH12_62', nk=4, valk=valk)
    endif
!
!
!------------RECUPERATION DES DIMENSIONS DES NUMEROTATIONS--------------
!
    call dismoi('NB_EQUA', numin, 'NUME_DDL', repi=neqin)
    call dismoi('NB_EQUA', numout, 'NUME_DDL', repi=neqout)
!
!-------------------ALLOCATION DU VECTEUR RESULTAT----------------------
!
    call wkvect(nomvec, base//' V I', neqout, ldcvn)
!
!
!-----------REQUETTE DES DEEQ DES NUMEROTATIONS-------------------------
!
    call jeveuo(numin//'.DEEQ', 'L', vi=nlldein)
    call jeveuo(numout//'.DEEQ', 'L', vi=nlldeou)
!
!
!------------------BOUCLE SUR LES DDL-----------------------------------
!
    do i = 1, neqout
        nuno=nlldeou(1+2*(i-1))
        ityp=nlldeou(1+2*(i-1)+1)
        if (ityp .gt. 0) then
            call cheddl(nlldein, neqin, nuno, ityp, iran,&
                        1)
            if (iran(1) .eq. 0) then
                erreur=.true.
                vali (1) = nuno
                vali (2) = ityp
                call utmess('A', 'ALGORITH12_63', ni=2, vali=vali)
            else
                zi(ldcvn+i-1)=iran(1)
            endif
        endif
!
    end do
!
!--------------------------TRAITEMENT ERREUR EVENTUELLE----------------
!
    if (erreur) then
        call utmess('F', 'ALGORITH12_64')
    endif
!
!------------------------LIBERATION DES OBJETS -------------------------
!
!
    call jedema()
end subroutine
