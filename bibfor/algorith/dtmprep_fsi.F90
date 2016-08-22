subroutine dtmprep_fsi(sd_dtm_)
    implicit none
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: hassan.berro at edf.fr
!
! dtmprep_fsi : Retreives information regarding the Fluid-Structure Interaction
!               configuration if needed for the dynamic integration.
!               This is verified by the presence of BASE_ELAS_FLUI
!
!   Note : Information about IFS configuration is read using mdconf and saved directly 
!          is sd_dtm
!
#include "jeveux.h"
#include "asterc/r8depi.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/dtmget.h"
#include "asterfort/dtminivec.h"
#include "asterfort/dtmsav.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mdconf.h"
#include "asterfort/utmess.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
!
!   -0.1- Input/output arguments
    character(len=*)          , intent(in) :: sd_dtm_
!
!   -0.2- Local variables
    integer          :: iret, jremf, jfsic, ivtflu, jvite
    integer          :: nbm, ifm, info, fsichoc, itypfl(2)
    integer          :: icoupl, nbno, nbmcfc, jnumo, jfreq
    integer          :: jmasg, i, j, im, nbmodd
    real(kind=8)     :: deuxpi, vgap
    character(len=8) :: sd_dtm, basflu, typflu, nombm, noma
!
    real(kind=8)     , pointer :: c_flu(:)    => null()

    integer          , pointer :: icoupled(:) => null()
    integer          , pointer :: nuor(:)     => null()
    integer          , pointer :: ires(:)     => null()

    real(kind=8)     , pointer :: masg(:)     => null()
    real(kind=8)     , pointer :: rigg(:)     => null()
    real(kind=8)     , pointer :: amog(:)     => null()
    real(kind=8)     , pointer :: puls(:)     => null()
    real(kind=8)     , pointer :: puls2(:)    => null()
    real(kind=8)     , pointer :: profv(:)    => null()
    real(kind=8)     , pointer :: rhoe(:)     => null()
    real(kind=8)     , pointer :: phie(:)     => null()
    real(kind=8)     , pointer :: basf(:)     => null()
    real(kind=8)     , pointer :: absc(:)     => null()
    real(kind=8)     , pointer :: codim(:)    => null()
    real(kind=8)     , pointer :: poids(:)    => null()
!
!
!   -0.3- Initializations
    call jemarq()
    call infmaj()
    call infniv(ifm, info)
    deuxpi = r8depi()

    sd_dtm = sd_dtm_
    call dtmget(sd_dtm, _NB_MODES, iscal=nbm)
    call dtminivec(sd_dtm, _FSI_ZET0, nbm, vr=c_flu)
    do i = 1, nbm
        c_flu(i) = 0.d0
    end do

!
!   --- 1 - BASE_ELAS_FLUI, *wet* basis defining the FSI coupling
    call getvid(' ', 'BASE_ELAS_FLUI', scal=basflu, nbret=iret)
    fsichoc = 0
    if (iret.eq.0) goto 100
!
    ! write(*,*) "BASE_ELAS_FLUI = ", basflu

!
!   --- 2 - Characteristics of the configuration in study
    call jeveuo(basflu//'           .REMF', 'L', jremf)
    typflu = zk8(jremf)
    nombm  = zk8(jremf+1)

    call jeveuo(typflu//'           .FSIC', 'L', jfsic)
    itypfl(1) = zi(jfsic)
    icoupl = zi(jfsic+1)

    if (icoupl .ne. 1) then
        call utmess('A', 'ALGORITH5_55')
        goto 100
    end if
!
    if ((itypfl(1).ne.1).and.(itypfl(1).ne.2)) then
        call utmess('A', 'ALGORITH5_54')
        goto 100
    endif

    fsichoc = 1
!
!   --- 2 - Gap velocity, given by the velocity index in the wet basis
    call getvis(' ', 'NUME_VITE_FLUI', scal=ivtflu)
    call jeveuo(basflu//'           .VITE', 'L', jvite)
    vgap = zr(jvite+ivtflu-1)

    call dismoi('NOM_MAILLA', nombm, 'RESULTAT', repk=noma)
    call dismoi('NB_NO_MAILLA', noma, 'MAILLAGE', repi=nbno)
!
!   --- 3 - Recovering modal properties for the coupled modes from the wet basis concept
!
!   --- nbmcfc : number of coupled modes
    call jelira(basflu//'           .NUMO', 'LONMAX', nbmcfc)
    call jeveuo(basflu//'           .NUMO', 'L'     , jnumo)

    call jeveuo(basflu//'           .FREQ', 'L', jfreq)
    call jeveuo(basflu//'           .MASG', 'L', jmasg)
    call dtmget(sd_dtm, _MASS_DIA, vr=masg)
    call dtmget(sd_dtm, _RIGI_DIA, vr=rigg)
    call dtmget(sd_dtm, _AMOR_DIA, vr=amog)
    call dtmget(sd_dtm, _OMEGA   , vr=puls, lonvec=nbmodd)
    call dtmget(sd_dtm, _OMEGA_SQ, vr=puls2)

    if (nbmodd.ne.nbm) then
        write(*,*) "FSI coupling is not compatible with dynamic substructuring"
        ASSERT(.false.)
    end if

    call dtminivec(sd_dtm, _FSI_CPLD, nbm, vi=icoupled)
    do i = 1, nbm
        icoupled(i) = 0
    end do

!   --- TODO : HERE WE CAN CHECK IF THE INPUT ZETA0 CORRESPONDS TO THAT GIVEN IN 
!              BASE_ELAS_FLUI AND DO THE CORRECTION TO THE COUPLING COEFFICIENTS 
!              IF REQUIRED SUCH AS FOR THE REDUCED DAMPING
    do j = 1, nbmcfc
        im = zi(jnumo+j-1)
        if (im .le. nbm) then
            icoupled(j) = 1
            puls (im) = deuxpi * zr(jfreq+2*(j-1) +2*nbmcfc*( ivtflu-1))
            puls2(im) = puls(im)*puls(im)
            masg (im) = zr(jmasg+j-1)
            rigg (im) = masg(im)*puls2(im)
!           --- Save the *pure* fluid damping for later use in case of FSI coupling during chocs
            c_flu(im) = 2.0d0 * zr(jfreq+2*(j-1)+2*nbmcfc*(ivtflu-1)+1)*puls(im) - amog(im)
            amog (im) = amog (im) + c_flu(im)
        endif
    end do

    AS_ALLOCATE(vi=nuor, size=nbm)
    do i = 1, nbm
        nuor(i) = i
    end do

    if (itypfl(1).eq.1) then
        call dtminivec(sd_dtm, _FSI_IRES, nbno    , vi=ires)
        call dtminivec(sd_dtm, _FSI_PRVI, 2*nbno+1, vr=profv)
        call dtminivec(sd_dtm, _FSI_RHOE, 2*nbno  , vr=rhoe)
        call dtminivec(sd_dtm, _FSI_BASF, nbm*nbno, vr=basf)
        call dtminivec(sd_dtm, _FSI_PHIE, 2       , vr=phie)
        call dtminivec(sd_dtm, _FSI_ABSC, nbno    , vr=absc)
        call mdconf(typflu, nombm, noma, nbm, nbno,&
                    nuor, 1, itypfl(2), ires, profv,&
                    rhoe, basf, phie, absc)
    else if (itypfl(1).eq.2) then
        call dtminivec(sd_dtm, _FSI_CODM, 4    , vr=codim)
        call dtminivec(sd_dtm, _FSI_POID, 2*nbm, vr=poids)
        call dtminivec(sd_dtm, _FSI_PHIE, 1    , vr=phie)
        call mdconf(typflu, nombm, noma, nbm, nbno,&
                    nuor, 0, itypfl(2), zi(1), masg,&
                    codim, poids, phie, zr(1))
    end if

    call dtmsav(sd_dtm, _FSI_BASE, 1, kscal=basflu)
    call dtmsav(sd_dtm, _FSI_TYPF, 1, kscal=typflu)
    call dtmsav(sd_dtm, _FSI_ITYP, 2, ivect=itypfl)
    call dtmsav(sd_dtm, _FSI_VGAP, 1, rscal=vgap)
    AS_DEALLOCATE(vi=nuor)

100 continue

!   --- Note that FSI_CASE is used to identify if FSI are to be considered
    call dtmsav(sd_dtm, _FSI_CASE, 1, iscal=fsichoc)

    call jedema()
end subroutine
