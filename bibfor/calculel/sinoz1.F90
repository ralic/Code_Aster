subroutine sinoz1(modele, sigma, signo)
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
!
!     ARGUMENTS:
!     ----------
! ......................................................................
!     BUT:
!           CALCUL DES CONTRAINTES AUX NOEUDS PAR LA METHODE ZZ1
!     ENTREES:
!        MODELE : NOM DU MODELE
!        SIGMA  : NOM DU CHAMP DE CONTRAINTES AUX POINTS DE GAUSS
!        SIGNO  : NOM DU CHAMP DE CONTRAINTES AUX NOEUDS
!
!----------------------------------------------------------------------
!
! ----------------------- DECLARATIONS --------------------------------
!
#include "jeveux.h"
!
#include "asterfort/asasve.h"
#include "asterfort/asmatr.h"
#include "asterfort/assert.h"
#include "asterfort/crcnct.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/me2zme.h"
#include "asterfort/memzme.h"
#include "asterfort/numer2.h"
#include "asterfort/numoch.h"
#include "asterfort/preres.h"
#include "asterfort/resoud.h"
#include "asterfort/u2mess.h"
    character(len=1) :: typres, k1bid
    character(len=8) :: modele
    character(len=14) :: nupgm
    character(len=8) :: kbid
    character(len=8) :: licmp(6), ma
    character(len=19) :: infcha
    character(len=19) :: solveu, vecele, matpre, k19bid, criter
    character(len=24) :: kmoch, signo, sigma, massel
    character(len=24) :: nume, vecass, vect(6)
    real(kind=8) :: rcmp(6)
    integer :: ibid, jvect, nbcmp, repdim
    complex(kind=8) :: cbid
    integer :: iret
!
!
!-----------------------------------------------------------------------
    integer :: i, ieq, ier, indeq, jkmoch, jnueq, jprno
    integer :: jsig, jsixx, jsixy, jsixz, jsiyy, jsiyz, jsizz
    integer :: jslvi, jvecas, nbligr, nbno
!-----------------------------------------------------------------------
    call jemarq()
!
    call dismoi('F', 'DIM_GEOM', modele, 'MODELE', repdim,&
                kbid, ibid)
!
    if ((repdim.ne.2) .or. (repdim.ne.3)) then
        if (repdim .eq. 1) call u2mess('F', 'CALCULEL_75')
        if (repdim .gt. 3) call u2mess('F', 'CALCULEL_76')
    endif
!
    if (repdim .eq. 2) then
        nbcmp = 4
    else if (repdim.eq.3) then
        nbcmp = 6
    else
        ASSERT(.false.)
    endif
!
    massel='&&MASSEL'
!
!     CALCUL DE LA MATRICE DE MASSE ZZ1 (A 1 CMP)
    call memzme(modele, massel(1:19))
!
    typres = 'R'
!
!
!     --  APPEL A NUMER2 POUR CONSTRUIRE UN NUME_DDL
!         SUR LA GRANDEUR SIZZ_R (1 CMP)
    nupgm = '&&NUME'
    kmoch = nupgm//'.&LMODCHAR'
    infcha = '&&SINOZ1.INFCHA'
!
!     -- CREATION DU SOLVEUR :
    solveu = '&&OP0042.SOLVEUR'
!
    call numoch(massel, 1, 'V', kmoch)
    call jeveuo(kmoch, 'L', jkmoch)
    call jelira(kmoch, 'LONUTI', nbligr)
    call numer2(' ', nbligr, zk24(jkmoch), 'DDL_NOZ1', solveu,&
                'VV', nupgm, ibid)
    call jedetr(kmoch)
!
    call asmatr(1, massel, ' ', nupgm, solveu,&
                infcha, 'ZERO', 'V', 1, '&&MASSAS')
!
!     CALCUL DES SECONDS MEMBRES
    vecele = '&&VECELE'
    call me2zme(modele, sigma(1:19), vecele)
!
!
!     ASSEMBLAGE DES SECONDS MEMBRES
    nume = '&&NUME'
    vecass = '??????'
    call asasve(vecele, nume, typres, vecass)
    call jeveuo(vecass, 'L', jvecas)
    do 10,i = 1,nbcmp
    vect(i) = zk24(jvecas-1+i)
    10 end do
!
!
!     RESOLUTIONS SANS DIRICHLET
!     -- ON FORCE STOP_SINGULIER='NON' MAIS POURQUOI ??
    call jeveuo(solveu//'.SLVI', 'E', jslvi)
    zi(jslvi-1+3)=1
    matpre='&&SINOZ1.MATPRE'
    call preres(solveu, 'V', ier, matpre, '&&MASSAS',&
                ibid, -9999)
!
    k19bid=' '
    k1bid=' '
    criter=' '
    do 20 i = 1, nbcmp
        call jeveuo(vect(i) (1:19)//'.VALE', 'E', jvect)
        call resoud('&&MASSAS', matpre, solveu, k19bid, 1,&
                    k19bid, k19bid, k1bid, zr(jvect), cbid,&
                    criter, .true., 0, iret)
20  end do
!
!   CREATION DU CHAM_NO_SIEF_R A PARTIR DES 4 CHAM_NO_SIZZ_R (A 1 CMP)
!
    do 30 i = 1, nbcmp
        rcmp(i) = 0.d0
30  end do
    licmp(1) = 'SIXX'
    licmp(2) = 'SIYY'
    licmp(3) = 'SIZZ'
    licmp(4) = 'SIXY'
    licmp(5) = 'SIXZ'
    licmp(6) = 'SIYZ'
    call dismoi('F', 'NOM_MAILLA', sigma(1:19), 'CHAM_ELEM', ibid,&
                ma, ier)
    call crcnct('G', signo, ma, 'SIEF_R', nbcmp,&
                licmp, rcmp)
    call jeveuo(signo(1:19)//'.VALE', 'E', jsig)
    call jeveuo(vect(1) (1:19)//'.VALE', 'E', jsixx)
    call jeveuo(vect(2) (1:19)//'.VALE', 'E', jsiyy)
    call jeveuo(vect(3) (1:19)//'.VALE', 'E', jsizz)
    call jeveuo(vect(4) (1:19)//'.VALE', 'E', jsixy)
    if (nbcmp .eq. 6) then
        call jeveuo(vect(5) (1:19)//'.VALE', 'E', jsixz)
        call jeveuo(vect(6) (1:19)//'.VALE', 'E', jsiyz)
    endif
    call jeveuo(jexnum(nume(1:14)//'.NUME.PRNO', 1), 'L', jprno)
    call jeveuo(nume(1:14)//'.NUME.NUEQ', 'L', jnueq)
!
    call dismoi('F', 'NB_NO_MAILLA', ma, 'MAILLAGE', nbno,&
                kbid, ier)
    do 40 i = 1, nbno
        indeq = zi(jprno-1+3* (i-1)+1)
        ieq = zi(jnueq-1+indeq)
        zr(jsig-1+nbcmp* (i-1)+1) = zr(jsixx-1+ieq)
        zr(jsig-1+nbcmp* (i-1)+2) = zr(jsiyy-1+ieq)
        zr(jsig-1+nbcmp* (i-1)+3) = zr(jsizz-1+ieq)
        zr(jsig-1+nbcmp* (i-1)+4) = zr(jsixy-1+ieq)
        if (nbcmp .eq. 6) then
            zr(jsig-1+nbcmp* (i-1)+5) = zr(jsixz-1+ieq)
            zr(jsig-1+nbcmp* (i-1)+6) = zr(jsiyz-1+ieq)
        endif
40  end do
!
    call detrsd('MATR_ASSE', '&&MASSAS')
    call jedetr(nupgm//'.&LMODCHAR')
    call detrsd('NUME_DDL', nupgm)
!
    do 50,i = 1,nbcmp
    call detrsd('CHAMP_GD', zk24(jvecas-1+i))
    50 end do
    call jedetr(vecass)
!
!
    call jedema()
end subroutine
