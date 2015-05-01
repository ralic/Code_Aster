subroutine te0481(nomopt, nomte)
! aslint: disable=W0104
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/dfdm3d.h"
#include "asterfort/elref1.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/iselli.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/reeref.h"
#include "asterfort/teattr.h"
#include "asterfort/vecini.h"
#include "asterfort/xteini.h"
    character(len=16) :: nomopt, nomte
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
! person_in_charge: patrick.massin at edf.fr
!
!.......................................................................
!
!      BUT : CALCULATION OF GAUSS POINTS COORDINATES FOR X-FEM ELEMENTS 1D 2D 3D
!            BUT NOT BOUNDARY ELEMENTS
!
!      OPTION : COOR_ELGA
!
!      INPUT : OPTION (CALCULATION OPTION)
!              NOMTE : ELEMENT TYPE NAME
!.......................................................................
!
    character(len=8) :: elrefp, elrese(6), fami(6), enr
    real(kind=8) :: xg(3), coorse(81), jac, r
    integer :: ibid, ndim, nnop, nno, npg, ivf, ipoids
    integer :: jpmilt, irese
    integer :: jpintt, jcnset, jlonch, igeom, jcopg
    integer :: i, j, nse, ise, in, ino, ipg, kpg, idfde
    aster_logical :: axi
!
    data    elrese /'SE2','TR3','TE4','SE3','TR6','T10'/
    data    fami   /'BID','XINT','XINT','BID','XINT','XINT'/
!
    call jemarq()
!
!.......................................................................
!
!
!-----------------------------------------------------------------------
!     INITIALISATIONS
!-----------------------------------------------------------------------
!
!     ELEMENT DE REFERENCE PARENT : RECUP DE NDIM ET NNOP


    call elref1(elrefp)
    call elrefe_info(fami='RIGI', ndim=ndim, nno=nnop)
!
    axi = lteatt('AXIS','OUI')
!
!     SOUS-ELEMENT DE REFERENCE : RECUP DE NNO, NPG ET IVF
    if (.not.iselli(elrefp)) then
        irese=3
    else
        irese=0
    endif
    call elrefe_info(elrefe=elrese(ndim+irese), fami=fami(ndim+irese),&
                     nno=nno, npg=npg, jvf=ivf, jdfde=idfde, jpoids=ipoids)
!
!-----------------------------------------------------------------------
!     RECUPERATION DES ENTREES / SORTIE
!-----------------------------------------------------------------------
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PPINTTO', 'L', jpintt)
    call jevech('PCNSETO', 'L', jcnset)
    call jevech('PLONCHA', 'L', jlonch)
    call jevech('PCOORPG', 'E', jcopg)
!     PROPRES AUX ELEMENTS 1D ET 2D (QUADRATIQUES)
    call teattr('S', 'XFEM', enr, ibid)
    if ((ibid.eq.0) .and. (.not. axi) .and.&
        (enr.eq.'XH' .or.enr.eq.'XHT'.or.enr.eq.'XT'.or.enr.eq.'XHC') .and. .not.iselli(elrefp)) &
    call jevech('PPMILTO', 'L', jpmilt)
!
!     RÉCUPÉRATION DE LA SUBDIVISION DE L'ÉLÉMENT EN NSE SOUS ELEMENT
    nse=zi(jlonch-1+1)
!
!       BOUCLE D'INTEGRATION SUR LES NSE SOUS-ELEMENTS
    do ise = 1, nse
!
!       BOUCLE SUR LES SOMMETS DU SOUS-TRIA (DU SOUS-SEG)
        do in = 1, nno
            ino=zi(jcnset-1+nno*(ise-1)+in)
            do j = 1, ndim
                if (ino .lt. 1000) then
                    coorse(ndim*(in-1)+j)=zr(igeom-1+ndim*(ino-1)+j)
                else if (ino.gt.1000 .and. ino.lt.2000) then
                    coorse(ndim*(in-1)+j)=zr(jpintt-1+ndim*(ino-1000-&
                    1)+j)
                else if (ino.gt.2000 .and. ino.lt.3000) then
                    coorse(ndim*(in-1)+j)=zr(jpmilt-1+ndim*(ino-2000-&
                    1)+j)
                else if (ino.gt.3000) then
                    coorse(ndim*(in-1)+j)=zr(jpmilt-1+ndim*(ino-3000-&
                    1)+j)
                endif
            enddo
        enddo
!
!-----------------------------------------------------------------------
!         BOUCLE SUR LES POINTS DE GAUSS DU SOUS-ELT
!-----------------------------------------------------------------------
!
        do kpg = 1, npg
!
!         COORDONNÉES DU PT DE GAUSS DANS LE REPÈRE RÉEL : XG
            call vecini(ndim, 0.d0, xg)
            do i = 1, ndim
                do in = 1, nno
                    xg(i) = xg(i) + zr(ivf-1+nno*(kpg-1)+in) * coorse( ndim*(in-1)+i)
                enddo
            enddo

!           CALCULER LE JACOBIEN DE LA TRANSFO SSTET->SSTET REF
!           AVEC LES COORDONNEES DU SOUS-ELEMENT
            if (ndim .eq. 2) then
                call dfdm2d(nno, kpg, ipoids, idfde, coorse,&
                            jac)
            else if (ndim.eq.3) then
                call dfdm3d(nno, kpg, ipoids, idfde, coorse,&
                            jac)
            endif
!
! -         CALCUL DE LA DISTANCE A L'AXE (AXISYMETRIQUE):
            if (axi) then
                r=xg(1)
!
                ASSERT(r.gt.0d0)
!               MODIFICATION DU JACOBIEN
                jac = jac * r
            endif

!         NUMERO DE CE POINT DE GAUSS DANS LA FAMILLE 'XFEM'
            ipg= (ise-1) * npg + kpg
!
            do i=1,ndim
                zr(jcopg+(ndim+1)*(ipg-1)-1+i)=xg(i)
            enddo    
            zr(jcopg+(ndim+1)*(ipg-1)-1+ndim+1)=jac
!
        enddo
!
!-----------------------------------------------------------------------
!         FIN DE LA BOUCLE SUR LES POINTS DE GAUSS DU SOUS-ELT
!-----------------------------------------------------------------------
!
    enddo
!
    call jedema()
end subroutine
