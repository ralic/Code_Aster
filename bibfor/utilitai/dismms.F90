subroutine dismms(questi, nomobz, repi, repkz, ierd)
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
    implicit none
!     --     DISMOI(MATR_ASSE) (MARCHE AUSSI PARFOIS SUR MATR_ASSE_GENE)
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismgd.h"
#include "asterfort/dismme.h"
#include "asterfort/dismnu.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
!
    integer :: repi, ierd
    character(len=*) :: questi
    character(len=*) :: nomobz, repkz
! ----------------------------------------------------------------------
!    IN:
!       QUESTI : TEXTE PRECISANT LA QUESTION POSEE
!       NOMOBZ : NOM D'UN OBJET DE CONCEPT MATR_ASSE  (K19)
!    OUT:
!       REPI   : REPONSE ( SI ENTIERE )
!       REPKZ  : REPONSE ( SI CHAINE DE CARACTERES )
!       IERD   : CODE RETOUR (0--> OK, 1 --> PB)
!
! ----------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    character(len=32) :: repk
    character(len=24) :: p1, p2, k24
    character(len=19) :: nomob, solveu
    character(len=2) :: typmat
!-----------------------------------------------------------------------
    integer :: i, ibid, ier
    integer ::  nblime
    character(len=24), pointer :: lime(:) => null()
    character(len=24), pointer :: refa(:) => null()
    integer, pointer :: deeq(:) => null()
    character(len=24), pointer :: slvk(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
    repk = ' '
    repi = 0
    ierd = 0
!
    nomob = nomobz
    call jeveuo(nomob//'.REFA', 'L', vk24=refa)
!
!
    if (questi(1:9) .eq. 'NUM_GD_SI') then
        call dismnu(questi, refa(2)(1:14), repi, repk, ierd)
    else if (questi(1:9).eq.'NOM_GD_SI') then
        call dismnu('NOM_GD', refa(2)(1:14), repi, repk, ierd)
!
    else if (questi.eq.'TYPE_MATRICE') then
        typmat=refa(9)(1:2)
        if (typmat .eq. 'MS') then
            repk='SYMETRI'
        else if (typmat.eq.'MR') then
            repk='NON_SYM'
        else
            ASSERT(.false.)
        endif
!
    else if (questi.eq.'NB_EQUA') then
        call dismnu(questi, refa(2)(1:14), repi, repk, ierd)
!
    else if (questi.eq.'NOM_MODELE') then
        call dismnu(questi, refa(2)(1:14), repi, repk, ierd)
!
    else if (questi.eq.'NOM_MAILLA') then
        repk= refa(1)(1:8)
!
    else if (questi.eq.'NOM_NUME_DDL') then
        repk= refa(2)(1:14)
!
    else if (questi.eq.'EXIS_LAGR') then
        call jeexin(nomob//'.CONL', ier)
        if (ier .eq. 0) then
            repk = 'NON'
        else
            repk = 'OUI'
        endif

    else if (questi.eq.'XFEM') then
        repk=refa(17)

    else if (questi.eq.'XFEM_PC') then
        repk=refa(18)(1:19)

    else if (questi.eq.'XFEM_PC_INV') then
        repk=refa(16)(1:19)

    else if (questi.eq.'SOLVEUR') then
        if (refa(7) .ne. ' ') then
            repk=refa(7)
        else
            call dismnu(questi, refa(2)(1:14), repi, repk, ierd)
        endif
!
    else if (questi.eq.'METH_RESO'.or.questi.eq.'RENUM_RESO') then
        if (refa(7) .ne. ' ') then
            solveu=refa(7)(1:19)
            call jeveuo(solveu//'.SLVK', 'L', vk24=slvk)
            if (questi .eq. 'METH_RESO') then
                repk=slvk(1)
            else
                repk=slvk(4)
            endif
        else
            call dismnu(questi, refa(2)(1:14), repi, repk, ierd)
        endif
!
    else if (questi.eq.'PROF_CHNO') then
        repk= refa(2)(1:14)//'.NUME'
!
    else if (questi.eq.'NUME_EQUA') then
        repk= refa(2)(1:14)//'.NUME'
!
    else if (questi.eq.'PHENOMENE') then
        call dismnu(questi, refa(2)(1:14), repi, repk, ierd)
!
    else if (questi.eq.'SUR_OPTION') then
        repk= refa(4)(1:16)
!
    else if (questi.eq. 'MPI_COMPLET') then
        k24 = refa(11)
        ASSERT((k24.eq.'MPI_COMPLET') .or. ( k24.eq.'MPI_INCOMPLET') .or. (k24.eq.'MATR_DISTR'))
        if (k24 .eq. 'MPI_COMPLET') then
            repk='OUI'
        else
            repk='NON'
        endif
!
    else if (questi.eq. 'MATR_DISTR') then
        k24 = refa(11)
        ASSERT((k24.eq.'MPI_COMPLET') .or. ( k24.eq.'MPI_INCOMPLET') .or. (k24.eq.'MATR_DISTR'))
        if (k24 .eq. 'MATR_DISTR') then
            repk='OUI'
        else
            repk='NON'
        endif
!
        else if((questi.eq.'CHAM_MATER').or. (questi.eq.'CARA_ELEM'))&
    then
        call jeveuo(nomob//'.LIME', 'L', vk24=lime)
        call jelira(nomob//'.LIME', 'LONMAX', nblime)
        p1=' '
        p2=' '
        ier=0
        do i = 1, nblime
            if (lime(i) .eq. ' ') goto 1
            call dismme(questi, lime(i)(1:19), ibid, p1, ierd)
            if (p1 .ne. ' ') then
                if (p2 .eq. ' ') then
                    p2=p1
                else
                    if (p1 .ne. p2) ier=1
                endif
            endif
  1         continue
        end do
        if (ier .eq. 0) then
            repk=p2
        else
            repk=' '
            ierd=1
        endif
    else
        ierd=1
    endif
!
    repkz = repk
    call jedema()
end subroutine
