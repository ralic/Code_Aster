subroutine noeddl(nume, nbnoe, lnonoe, neq, ivec)
    implicit none
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/nbec.h"
#include "asterfort/utmess.h"
!
    integer :: nbnoe, neq, ivec(neq)
    character(len=14) :: nume
    character(len=*) :: lnonoe(nbnoe)
! ----------------------------------------------------------------------
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
! ----------------------------------------------------------------------
! IN : NUME   : NOM D'UN NUME_DDL
! IN : NBNOE  : NOMBRE DE NOEUD DE LA LISTE LNONOE
! IN : LNONOE : LISTE DE NOMS DE NOEUD
! IN : NEQ    : NOMBRE D'EQUATIONS DE NUME
! IN : IVEC   : VECTEUR DE POINTEURS DE DDLS DEJA ALLOUE.
!
!     OUT:
!     IVEC EST REMPLI DE 0 OU DE 1
!     IVEC(IEQ) =
!       - 1 SI LE IEQ-EME NOEUD DE NUME A POUR NOM: LNONOE(INOE)
!       - 0 SINON
! ----------------------------------------------------------------------
    integer :: gd
    character(len=8) :: nomma, nomno, k8bid
    character(len=24) :: nomnu
    character(len=24) :: valk(2)
! ----------------------------------------------------------------------
!
!     - MISE A ZERO DE IVEC:
!     - NON NECESSAIRE, L'OBJET EST CREE/DETRUIT A CHAQUE FOIS
!     - DANS MSTGET
!-----------------------------------------------------------------------
    integer :: i, ianueq, iaprno, ibid, ieq, ierd, in
    integer :: nbcmp, nec, nunoe
!-----------------------------------------------------------------------
    call jemarq()
!      DO 10 I = 1,NEQ
!          DO 10 J = 1,NBNOE
!              IVEC(I,J) = 0
! 10   CONTINUE
!
    nomnu(1:14) = nume
    nomnu(15:19) = '.NUME'
    call jeveuo(nomnu(1:19)//'.NUEQ', 'L', ianueq)
    call dismoi('F', 'NOM_MAILLA', nume, 'NUME_DDL', ibid,&
                nomma, ierd)
    call dismoi('F', 'NUM_GD_SI', nume, 'NUME_DDL', gd,&
                k8bid, ierd)
    nec = nbec(gd)
    call jenonu(jexnom(nomnu(1:19)//'.LILI', '&MAILLA'), ibid)
    call jeveuo(jexnum(nomnu(1:19)//'.PRNO', ibid), 'L', iaprno)
!
    do 20 in = 1, nbnoe
        nomno = lnonoe(in)
        call jenonu(jexnom(nomma//'.NOMNOE', nomno), nunoe)
        if (nunoe .eq. 0) then
            valk (1) = nomno
            valk (2) = nomma
            call utmess('E', 'UTILITAI6_47', nk=2, valk=valk)
        endif
        ieq = zi(iaprno-1+(nec+2)*(nunoe-1)+1)
        nbcmp = zi(iaprno-1+(nec+2)*(nunoe-1)+2)
        do 22 i = 1, nbcmp
            ivec(ieq+i-1) = 1
22      continue
20  end do
!
    call jedema()
end subroutine
