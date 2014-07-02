subroutine chrpan(modele, carte, option, chelem)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/indik8.h"
#include "asterc/r8pi.h"
#include "asterfort/angvx.h"
#include "asterfort/assert.h"
#include "asterfort/carces.h"
#include "asterfort/cescel.h"
#include "asterfort/cesexi.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeveuo.h"
#include "asterfort/reliem.h"
    character(len=*), intent(in) :: modele, carte, chelem, option
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     COMMANDE MODI_REPERE :
!     SURCHARGE ALPHA, BETA, AXE, ORIG  DANS LA CARTE '.CAORIE'
!     ------------------------------------------------------------------
! IN  : MODELE : MODELE
! IN  : CARTE  : CARTE A TRANSFORMER EN CHAM ELEM
! IN  : OPTION : REPE_TENS ou REPE_GENE 
! OUT : CHELEM : CHAM ELEM AVEC ANGLES EVENTUELLEMENT VARIABLES
!     ------------------------------------------------------------------
!
    integer :: ibid, ioc, n1, n2, na, nvec, iret, nrep, nbma, nbmail, jmail
    integer :: ialpha, ibeta, iad1, iad2, ima, numma, ncmax, icesl
    integer :: iaxe(3), io(3), iad(3), ii
    integer :: icesd, nncp
    real(kind=8) :: ang(2), vect(3), axez(3), orig(3)
    aster_logical :: ltout
    character(len=8) :: k8b, noma, motcls(2), typmcl(2)
    character(len=19) :: chelms
    character(len=24) :: mesmai, ligrmo
    character(len=8), pointer :: cesk(:) => null()
    real(kind=8), pointer :: cesv(:) => null()
    character(len=8), pointer :: cesc(:) => null()
! --- ------------------------------------------------------------------
    call getfac('AFFE', nrep)
    if (nrep .eq. 0) goto 999
! --- ------------------------------------------------------------------
! --- PASSAGE PAR UN CHAM_ELEM_S
    k8b = ' '
    chelms = '&&CHRPAN.ELEM_S  '
    call carces(carte, 'ELEM', k8b, 'V', chelms,&
                'A', iret)
!
    call jeveuo(chelms//'.CESK', 'L', vk8=cesk)
    call jeveuo(chelms//'.CESC', 'L', vk8=cesc)
    call jeveuo(chelms//'.CESD', 'L', icesd)
    call jeveuo(chelms//'.CESL', 'E', icesl)
    call jeveuo(chelms//'.CESV', 'E', vr=cesv)
!
    noma = cesk(1)
    nbmail = zi(icesd)
    ncmax = zi(icesd+1)
! --- ------------------------------------------------------------------
! --- INDICE DE 'ALPHA' ET 'BETA' DANS LA CARTE
    ialpha = indik8 ( cesc, 'ALPHA   ', 1, ncmax )
    ibeta = indik8 ( cesc, 'BETA    ', 1, ncmax )
    iaxe(1) = indik8 ( cesc, 'AXE_X   ', 1, ncmax )
    iaxe(2) = indik8 ( cesc, 'AXE_Y   ', 1, ncmax )
    iaxe(3) = indik8 ( cesc, 'AXE_Z   ', 1, ncmax )
    io(1) = indik8 ( cesc, 'O_X     ', 1, ncmax )
    io(2) = indik8 ( cesc, 'O_Y     ', 1, ncmax )
    io(3) = indik8 ( cesc, 'O_Z     ', 1, ncmax )
    ASSERT(ialpha.eq.1.and.ibeta.eq.2)
!
    motcls(1) = 'GROUP_MA'
    motcls(2) = 'MAILLE'
    typmcl(1) = 'GROUP_MA'
    typmcl(2) = 'MAILLE'
    mesmai = '&&CHRPAN.MES_MAILLES'
!
! --- ------------------------------------------------------------------
    do ioc = 1, nrep
        call getvtx('AFFE', 'MAILLE', iocc=ioc, nbval=0, nbret=n1)
        call getvtx('AFFE', 'GROUP_MA', iocc=ioc, nbval=0, nbret=n2)
        if (n1+n2 .eq. 0) then
! Pas de filtre : toutes les mailles sont concernées
            ltout = .true.
            nbma = nbmail
        else
! Création de l'objet jeveux "mesmai" contenant la liste des numéros 
! des nbma mailles concernés 
            call reliem(' ', noma, 'NU_MAILLE', 'AFFE', ioc,&
                        2, motcls, typmcl, mesmai, nbma)
            if (nbma .ne. 0) call jeveuo(mesmai, 'L', jmail)
            ltout = .false.
        endif
!
!       Lecture des deux angles nautiques définissant le nouveau repère utilisateur 
        ang(1) = 0.d0
        ang(2) = 0.d0
        call getvr8('AFFE', 'ANGL_REP', iocc=ioc, nbval=2, vect=ang,&
                    nbret=na)
!        ou bien d' un vecteur 
        call getvr8('AFFE', 'VECTEUR', iocc=ioc, nbval=3, vect=vect,&
                    nbret=nvec)
        if (nvec .ne. 0) then
!       à partir duquel on calcule les deux angles nautiques 
            call angvx(vect, ang(1), ang(2))
            ang(1)= ang(1) * 180.d0/r8pi()
            ang(2)= ang(2) * 180.d0/r8pi() 
        endif
!
!      Lecture de l'axe et de l'origine du repère cylindrique 
!
        orig(:) = 0.d0
        axez(:) = 0.d0
        call getvr8('AFFE', 'ORIGINE', iocc=ioc, nbval=3, vect=orig,&
                    nbret=ibid)
        call getvr8('AFFE', 'AXE_Z', iocc=ioc, nbval=3, vect=axez,&
                    nbret=ibid)
        do ima = 1, nbma
            if (ltout) then
                numma = ima
            else
                numma = zi(jmail+ima-1)
            endif
! ALPHA
            call cesexi('C', icesd, icesl, numma, 1,&
                        1, ialpha, iad1)
            if (iad1 .lt. 0) then
                iad1 = -iad1
                zl(icesl-1+iad1) = .true.
            endif 
            cesv(iad1) = ang(1)
! BETA
            call cesexi('C', icesd, icesl, numma, 1,&
                        1, ibeta, iad2)
            if (iad2 .lt. 0) then
                iad2 = -iad2
                zl(icesl-1+iad2) = .true.
            endif 
            cesv(iad2) = ang(2)
! AXE (3 coordonnées)
            do ii = 1, 3
                call cesexi('C', icesd, icesl, numma, 1,&
                            1, iaxe(ii), iad(ii))
                if (iad(ii) .lt. 0) then
                    iad(ii) = -iad(ii)
                    zl(icesl-1+iad(ii)) = .true.
                endif 
                cesv(iad(ii)) = axez(ii)
            enddo
! ORIG (3 coordonnées)
            do ii = 1, 3
                call cesexi('C', icesd, icesl, numma, 1,&
                            1, io(ii), iad(ii))
                if (iad(ii) .lt. 0) then
                    iad(ii) = -iad(ii)
                    zl( icesl-1+iad(ii) ) = .true.
                endif 
                cesv(iad(ii)) = orig(ii)
            enddo
!
        end do
!
        if (.not. ltout) call jedetr(mesmai)
!
    end do
!
    call dismoi('NOM_LIGREL', modele, 'MODELE', repk=ligrmo)
! Création d'un cham_elem à partir du champ simple 
! Attention les modes locaux de CAORIE dans les catalogues de REPE_TENS et 
! REPE_GENE sont différents 
    call cescel(chelms, ligrmo, option, 'PANGREP', 'NON',&
                nncp, 'V', chelem, 'F', ibid)
!
    ASSERT(ibid==0)
!
    call detrsd('CHAM_ELEM_S', chelms)
!
999 continue
end subroutine
