subroutine nmcalm(typmat, modelz, lischa, mate, carele,&
                  compor, instam, instap, carcri, valinc,&
                  solalg, optmaz, base, meelem, defico,&
                  resoco, matele)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/meamme.h"
#include "asterfort/mecgm2.h"
#include "asterfort/mecgme.h"
#include "asterfort/medime.h"
#include "asterfort/memame.h"
#include "asterfort/memare.h"
#include "asterfort/merige.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmdebg.h"
#include "asterfort/nmelcm.h"
#include "asterfort/wkvect.h"
    character(len=*) :: modelz
    character(len=*) :: mate, carele
    character(len=24) :: compor, carcri
    real(kind=8) :: instam, instap
    character(len=24) :: defico, resoco
    character(len=19) :: lischa
    character(len=6) :: typmat
    character(len=*) :: optmaz
    character(len=1) :: base
    character(len=19) :: meelem(*), solalg(*), valinc(*)
    character(len=19) :: matele
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL)
!
! CALCUL DES MATRICES ELEMENTAIRES
!
! ----------------------------------------------------------------------
!
!
! IN  MODELE : NOM DU MODELE
! IN  LISCHA : LISTE DES CHARGEMENTS
! IN  MATE   : CHAMP MATERIAU
! IN  CARCRI : PARAMETRES DES METHODES D'INTEGRATION LOCALES
! IN  TYPMAT : TYPE DE MATRICE A CALCULER
!                MERIGI  - MATRICE POUR RIGIDITE
!                MEDIRI  - MATRICE POUR CL DIRICHLET LAGRANGE
!                MEGEOM  - MATRICE POUR NON-LIN. GEOMETRIQUE
!                MEAMOR  - MATRICE POUR AMORTISSEMENT
!                MEMASS  - MATRICE POUR MASSE
!                MESUIV  - MATRICE POUR CHARGEMENT SUIVEUR
!                MESSTR  - MATRICE POUR SOUS-STRUCTURES
!                MEELTC  - MATRICE POUR ELTS DE CONTACT
!                MEELTF  - MATRICE POUR ELTS DE FROTTEMENT
! IN  OPTCAL : OPTION DE CALCUL DU MATR_ELEM
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  OPTMAT : OPTION DE CALCUL POUR LA MATRICE
! OUT MATELE : MATRICE ELEMENTAIRE
!
!
!
!
    character(len=19) :: memass, merigi
    character(len=24) :: modele
    integer :: jinfc, jchar, jchar2, iarefe
    integer :: nbchar
    integer :: i
    character(len=16) :: optmat
    character(len=19) :: depmoi, sigplu, vitplu, vitmoi, accmoi, strplu
    character(len=19) :: depdel, varplu
    character(len=24) :: charge, infoch
    character(len=8) :: mailla
    integer :: ifm, niv
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- INITIALISATIONS
!
    optmat = optmaz
    modele = modelz
    call dismoi('NOM_MAILLA', modele, 'MODELE', repk=mailla)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE><MATR> CALCUL DES MATR_ELEM' //&
        ' DE TYPE <',typmat,'>'
    endif
!
! --- DECOMPACTION DES VARIABLES CHAPEAUX
!
    if (valinc(1)(1:1) .ne. ' ') then
        call nmchex(valinc, 'VALINC', 'DEPMOI', depmoi)
        call nmchex(valinc, 'VALINC', 'VITMOI', vitmoi)
        call nmchex(valinc, 'VALINC', 'ACCMOI', accmoi)
        call nmchex(valinc, 'VALINC', 'VITPLU', vitplu)
        call nmchex(valinc, 'VALINC', 'SIGPLU', sigplu)
        call nmchex(valinc, 'VALINC', 'STRPLU', strplu)
        call nmchex(valinc, 'VALINC', 'VARMOI', varplu)
    endif
    if (solalg(1)(1:1) .ne. ' ') then
        call nmchex(solalg, 'SOLALG', 'DEPDEL', depdel)
    endif
    if (meelem(1)(1:1) .ne. ' ') then
        call nmchex(meelem, 'MEELEM', 'MERIGI', merigi)
        call nmchex(meelem, 'MEELEM', 'MEMASS', memass)
    endif
!
! --- TRANSFO CHARGEMENTS
!
    charge = lischa(1:19)//'.LCHA'
    infoch = lischa(1:19)//'.INFC'
    call jeveuo(infoch, 'L', jinfc)
    nbchar = zi(jinfc)
    if (nbchar .ne. 0) then
        call jeveuo(charge, 'L', jchar)
        call wkvect('&&NMCALC.LISTE_CHARGE', 'V V K8', nbchar, jchar2)
        do i = 1, nbchar
            zk8(jchar2-1+i) = zk24(jchar-1+i) (1:8)
        end do
    else
        call wkvect('&&NMCALC.LISTE_CHARGE', 'V V K8', 1, jchar2)
    endif
!
    if (typmat .eq. 'MEDIRI') then
!
! --- MATR_ELEM DES CL DE DIRICHLET B
!
        call medime('V', 'ZERO', modele, lischa, matele)
!
! --- MATR_ELEM RIGIDITE GEOMETRIQUE
!
    else if (typmat.eq.'MEGEOM') then
        call detrsd('MATR_ELEM', matele)
        call merige(modele(1:8), carele(1:8), sigplu, strplu, matele,&
                    'V', 0)
!
! --- MATR_ELEM MASSES
!
    else if (typmat.eq.'MEMASS') then
        call memame(optmat, modele, nbchar, zk8(jchar2), mate,&
                    carele, .true., instam, compor, matele,&
                    base)
!
! --- MATR_ELEM AMORTISSEMENT
!
    else if (typmat.eq.'MEAMOR') then
        call meamme(optmat, modele, nbchar, zk8(jchar2), mate,&
                    carele, .true., instam, 'V', merigi,&
                    memass, matele, varplu)
!
! --- MATR_ELEM POUR CHARGES SUIVEUSES
!
    else if (typmat.eq.'MESUIV') then
        call mecgme(modele, carele, mate, lischa, instap,&
                    depmoi, depdel, instam, compor, carcri,&
                    matele)
        call mecgm2(lischa, instap, matele)
!
! --- MATR_ELEM DES SOUS-STRUCTURES
!
    else if (typmat.eq.'MESSTR') then
        call memare(base, matele, modele(1:8), mate, carele,&
                    optmat)
        call jeveuo(matele//'.RERR', 'E', iarefe)
        zk24(iarefe-1+3) = 'OUI_SOUS_STRUC'
!
! --- MATR_ELEM DES ELTS DE CONTACT (XFEM+CONTINUE)
!
    else if (typmat.eq.'MEELTC') then
        call nmelcm('CONT', modele, defico, resoco, mate,&
                    depmoi, depdel, vitmoi, vitplu, accmoi,&
                    matele)
!
! --- MATR_ELEM DES ELTS DE FROTTEMENT (XFEM+CONTINUE)
!
    else if (typmat.eq.'MEELTF') then
        call nmelcm('FROT', modele, defico, resoco, mate,&
                    depmoi, depdel, vitmoi, vitplu, accmoi,&
                    matele)
    else
        ASSERT(.false.)
    endif
!
! --- DEBUG
!
    if (niv .eq. 2) then
        call nmdebg(' ', matele, ifm)
    endif
!
! --- MENAGE
!
    call jedetr('&&NMCALC.LISTE_CHARGE')
    call jedema()
!
end subroutine
