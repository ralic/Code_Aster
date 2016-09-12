subroutine xkamat(imate, ndim, axi, ka, mu, famiz)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
! aslint: disable=W1504
use calcul_module, only : ca_jvcnom_, ca_nbcvrc_
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/utmess.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/lteatt.h"
#include "asterfort/rcvala.h"
#include "asterfort/rcvarc.h"
#include "asterfort/rcvad2.h"
!
    integer :: imate, ndim
    real(kind=8) :: ka, mu
    aster_logical :: axi
    character(len=*), optional  :: famiz
!
!    - FONCTION REALISEE : LECTURE DE PARAMETRES 'ELASTIQUES' AU PG
!
    integer :: nbpamx, ipar
    parameter (nbpamx=10)
    real(kind=8) :: e, nu, valres(2), valpar(nbpamx), valvrc, devres(2)
    integer :: icodre(2), iarret, ier, nbpar
    character(len=4)  :: fami
    character(len=16) :: novrc
    character(len=16) :: nomres(2), nompar(nbpamx)
    data     nomres /'E','NU'/
!
    call jemarq()
!
!   ABANDON DE L APPEL A RCVALD2 ==> ON PREFERE L APPEL A RCVALA QUI EST 
!     PLUS DIRECT ET PLUS ROBUSTE
    nbpar=1
    nompar(1)=' '
    valpar(1)=0.d0
    if (present(famiz)) then
      fami=famiz(1:4)
    else
      fami='XFEM'
    endif
!
    if (fami.eq.'XCON') goto 10
!
    if (ca_nbcvrc_ .eq. 0) goto 10
    nbpar=0
    do ipar=1,ca_nbcvrc_
        novrc=zk8(ca_jvcnom_-1+ipar)
        call rcvarc(' ', novrc, '+', fami, 1,&
                    1, valvrc, ier)
        if (ier .eq. 0) then
            nbpar=nbpar+1
            nompar(nbpar)=novrc
            valpar(nbpar)=valvrc
        endif
    enddo
!
10  continue
!
    if (fami.ne.'XCON') then
    call rcvala(imate, ' ', 'ELAS', nbpar, nompar(1:nbpar),&
                valpar(1:nbpar), 2, nomres, valres, icodre,&
                iarret, 'OUI')
    else
    call rcvad2('XFEM', 1, 1, '+', imate,&
                'ELAS', 2, nomres, valres, devres,&
                icodre)
    endif
!
!  VERIFICATION DE LA PRESENCE DES PARAM. ELAS. 
   if (count(icodre.eq.0).ne.2) then
     call utmess('F', 'ELEMENTS6_10')
   endif
!
   e = valres(1)
   nu = valres(2)
!
   if (ndim.eq.2) then
!
      if (lteatt('C_PLAN','OUI')) then
            ka = (3.d0-nu)/(1.d0+nu)
            mu = e/(2.d0*(1.d0+nu))
      else
            mu = e/(2.d0*(1.d0+nu))
            ka = 3.d0-4.d0*nu 
      endif
!
   else
!
            mu = e/(2.d0*(1.d0+nu))
            ka = 3.d0-4.d0*nu
!
   endif
!
    call jedema()
end subroutine
