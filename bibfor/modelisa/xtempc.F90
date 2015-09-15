subroutine xtempc(nfiss, fiss, fonree, char)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
#include "jeveux.h"
#include "asterfort/aflrch.h"
#include "asterfort/agdual.h"
#include "asterfort/afrela.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
    integer :: nfiss
    character(len=8) :: fiss(nfiss), char
    character(len=4) :: fonree
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM (AFFE_CHAR_THER)
!
! ANNULER LES DDLS X-FEM ASSOCIES AUX FISSURES FISS(1:NFISS)
! -> AFFE_CHAR_THER   / ECHANGE_PAROI / FISSURE / TEMP_CONTINUE
! -> AFFE_CHAR_THER_F / ECHANGE_PAROI / FISSURE / TEMP_CONTINUE
!
! ----------------------------------------------------------------------
!
! IN     NFISS  : NOMBRE DE FISSURES
! IN     FISS   : LISTE DES NOMS DES FISSURES
! IN     FONREE : 'REEL' OU 'FONC'
! IN-OUT CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
!
! ----------------------------------------------------------------------
!
    character(len=19) :: lisrel, stano
    character(len=8) :: noma, nomo, betaf, nomnoe(1), ddlh(1), ddle(1)
    character(len=4) :: typval
    complex(kind=8) :: cbid
    integer :: nrel, ifiss,  nbno, ino, istan, ndim(1)
    real(kind=8) :: betar, coefr(1)
    integer, pointer :: vale(:) => null()
!
    data ddlh /'H1'/
    data ddle /'E1'/
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    lisrel = '&&CXTEMPC.RLISTE'
    nrel = 0
!
! --- INITIALISATION DES ARGUMENTS IN COMMUNS POUR APPEL A AFRELA
!
    ndim(1) = 0
    coefr(1) = 1.d0
    ASSERT(fonree.eq.'REEL' .or. fonree.eq.'FONC')
    typval = fonree
    betar = 0.d0
    betaf = '&FOZERO'
!
! --- MAILLAGE ET MODELE
!
    call dismoi('NOM_MODELE', char(1:8), 'CHARGE', repk=nomo)
    call dismoi('NOM_MAILLA', nomo, 'MODELE', repk=noma)
    call dismoi('NB_NO_MAILLA', noma, 'MAILLAGE', repi=nbno)
!
!     ---------------------------------------
! --- BOUCLE SUR LES FISSURES
!     ---------------------------------------
!
    do ifiss = 1, nfiss
!
        stano=fiss(ifiss)//'.STNO'
        call jeveuo(stano//'.VALE', 'L', vi=vale)
!
!       -------------------------------------
! ----- BOUCLE SUR LES NOEUDS DU MAILLAGE
!       -------------------------------------
!
        do ino = 1, nbno
            istan = vale(ino)
            if (istan .gt. 0) then
                call jenuno(jexnum(noma(1:8)//'.NOMNOE', ino), nomnoe(1))
!
!           MISE A ZERO DDL HEAVISIDE
                if (istan .eq. 1) then
                    call afrela(coefr, [cbid], ddlh, nomnoe, ndim,&
                                [0.d0], 1, betar, cbid, betaf,&
                                'REEL', typval, '12', 0.d0, lisrel)
                    nrel = nrel + 1
!
!           MISE A ZERO DDL CRACK-TIP
                else if (istan.eq.2) then
                    call afrela(coefr, [cbid], ddle, nomnoe, ndim,&
                                [0.d0], 1, betar, cbid, betaf,&
                                'REEL', typval, '12', 0.d0, lisrel)
                    nrel = nrel + 1
!
!           MISE A ZERO DDLS HEAVISIDE ET CRACK-TIP
                else if (istan.eq.3) then
                    call afrela(coefr, [cbid], ddlh, nomnoe, ndim,&
                                [0.d0], 1, betar, cbid, betaf,&
                                'REEL', typval, '12', 0.d0, lisrel)
                    call afrela(coefr, [cbid], ddle, nomnoe, ndim,&
                                [0.d0], 1, betar, cbid, betaf,&
                                'REEL', typval, '12', 0.d0, lisrel)
                    nrel = nrel + 2
                else
                    ASSERT(.false.)
                endif
!
            endif
!
        end do
!       -------------------------------------
! ----- FIN BOUCLE SUR LES NOEUDS DU MAILLAGE
!       -------------------------------------
    end do
!     ---------------------------------------
! --- FIN BOUCLE SUR LES FISSURES
!     ---------------------------------------
!
! --- AFFECTATION DES RELATIONS LINEAIRES DANS LE LIGREL DE CHARGE
!
    ASSERT(nrel.gt.0)
    call agdual(char,1,'LIN')
    call aflrch(lisrel, char)
!
    call jedema()
end subroutine
