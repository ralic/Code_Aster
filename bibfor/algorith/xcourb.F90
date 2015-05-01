subroutine xcourb(basloc, noma, modele, courb)
!
    implicit none
#include "jeveux.h"
#include "asterfort/calcul.h"
#include "asterfort/cnocns.h"
#include "asterfort/cnscno.h"
#include "asterfort/cnscre.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/normev.h"
#include "asterfort/provec.h"
    character(len=8) :: modele, noma
    character(len=24) :: basloc, courb
!
!
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
! person_in_charge: samuel.geniaut at edf.fr
! ----------------------------------------------------------------------
! FONCTION REALISEE:  CALCUL DE LA COURBURE (DERIVÉE DE LA MATRICE
!                       DE PASSAGE LOCAL-GLOBAL)
!
!    ENTREE
!      BASLOC    :   BASE LOCALE CONTENANT LES GRADIENTS DE LA LEVEL-SET
!      MODELE    :   NOM DE L'OBJET MODELE
!      NOMA      :   NOM DE L'OBJET MAILLAGE
!
!    SORTIE
!      COURB     :   NOM DU TENSEUR DE COURBURE
!.......................................................................
!
    integer :: ino, i, j, nbno, ibid, nchin
    integer ::  jrsl, jgl
    real(kind=8) :: el1(3), el2(3), el3(3), p(3, 3), invp(3, 3), norme
    character(len=8) :: lpain(2), lpaout(1), licmp(9)
    character(len=19) :: cnsr, matpas, cnsg
    character(len=24) :: lchin(2), lchout(1), ligrmo
    real(kind=8), pointer :: gb(:) => null()
    real(kind=8), pointer :: rsv(:) => null()
!
!
    call jemarq()
!
    cnsg='&&XCOURB.CNSGT'
    call cnocns(basloc, 'V', cnsg)
!
    call jeveuo(cnsg//'.CNSV', 'L', vr=gb)
    call jeveuo(cnsg//'.CNSL', 'L', jgl)
!
    call dismoi('NB_NO_MAILLA', noma, 'MAILLAGE', repi=nbno)
!
!------------------------------------------------------------------
!     CREATION DU CHAM_NO SIMPLE MATPASS  (MATRICE INVP)
!------------------------------------------------------------------
    cnsr='&&XCOURB.CNSLT'
    licmp(1) = 'X1'
    licmp(2) = 'X2'
    licmp(3) = 'X3'
    licmp(4) = 'X4'
    licmp(5) = 'X5'
    licmp(6) = 'X6'
    licmp(7) = 'X7'
    licmp(8) = 'X8'
    licmp(9) = 'X9'
    call cnscre(noma, 'NEUT_R', 9, licmp, 'V',&
                cnsr)
    call jeveuo(cnsr//'.CNSV', 'E', vr=rsv)
    call jeveuo(cnsr//'.CNSL', 'E', jrsl)
!
    do ino = 1, nbno
!       ON VÉRIFIE QUE LE NOEUD A BIEN UNE VALEUR DE GRADLST ASSOCIÉE
        if (.not.zl(jgl-1+3*3*(ino-1)+4)) goto 100
        do i = 1, 3
            el1(i) = gb(3*3*(ino-1)+i+3)
            el2(i) = gb(3*3*(ino-1)+i+6)
        end do
!
!       NORMALISATION DE LA BASE
        call normev(el1, norme)
        call normev(el2, norme)
        call provec(el1, el2, el3)
!       CALCUL DE LA MATRICE DE PASSAGE P TQ 'GLOBAL' = P * 'LOCAL'
        do i = 1, 3
            p(i,1)=el1(i)
            p(i,2)=el2(i)
            p(i,3)=el3(i)
        end do
!       CALCUL DE L'INVERSE DE LA MATRICE DE PASSAGE : INV=TP
        do i = 1, 3
            do j = 1, 3
                invp(i,j)=p(j,i)
            end do
        end do
        do i = 1, 3
            rsv(9*(ino-1)+i)=invp(i,1)
            zl(jrsl-1+9*(ino-1)+i)=.true.
            rsv(9*(ino-1)+i+3)=invp(i,2)
            zl(jrsl-1+9*(ino-1)+i+3)=.true.
            rsv(9*(ino-1)+i+6)=invp(i,3)
            zl(jrsl-1+9*(ino-1)+i+6)=.true.
        end do
!
100     continue
    end do
    matpas='&&XCOURB.MATPAS'
    call cnscno(cnsr, ' ', 'NON', 'V', matpas,&
                'F', ibid)
!
!------------------------------------------------------------------
!     CALCUL DU GRADIENT DE MATPASS : CHAM_ELGA A 27 COMPOSANTES
!------------------------------------------------------------------
!
    lpain(1)='PGEOMER'
    lchin(1)=noma//'.COORDO'
    lpain(2)='PNEUTER'
    lchin(2)=matpas
    lpaout(1)='PGNEUTR'
    lchout(1)=courb
    ligrmo=modele//'.MODELE'
    nchin=2
    call calcul('S', 'GRAD_NEUT9_R', ligrmo, nchin, lchin,&
                lpain, 1, lchout, lpaout, 'V',&
                'OUI')
!
    call jedema()
!
end subroutine
