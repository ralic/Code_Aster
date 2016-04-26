subroutine pgpcal(sd_pgp)
    implicit none
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
! Calculate the physical fields of interest based on the user requests
! and preprocessed data in the pgp data structure
!
! Saves the results into a standard Code_Aster table structure
!
! ----------------------------------------------------------------------
! person_in_charge: hassan.berro at edf.fr    
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/pgpget.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!   ====================================================================
!   = 0 =   Variable declarations and initialization
!   ====================================================================
!   -0.1- Input/output arguments
    character(len=8), intent(in):: sd_pgp
!   -0.2- Local variables
    real(kind=8)      :: physvalr
    complex(kind=8)   :: physvalc
    integer           :: nbobs, iobs, physlen, length, nbmodes
    integer           :: i, j, iord, dec1, nord
    integer           :: jevol, jvecr, jvecc, jtblp, lc
    character(len=4)  :: chreco, typcha, typsc, typres
    character(len=8)  :: resin, base, result
    character(len=12) :: bl11pt
    character(len=16) :: champ
    character(len=24) :: nomjv

    integer         , pointer :: lordr(:) => null()
    real(kind=8)    , pointer :: vectr(:) => null()
    complex(kind=8) , pointer :: vectc(:) => null()

!   ------------------------------------------------------------------------------------
!   Definition of statement functions giving the appropriate (i,j) term in the basis
!   vector
#define vr(m,n) vectr((n-1)*physlen+m)
#define evolr(n,p) zr(jevol+(p-1)*nbmodes+n-1)
#define vc(m,n) vectc((n-1)*physlen+m)
#define evolc(n,p) zc(jevol+(p-1)*nbmodes+n-1)

!   -0.3- Initialization

    bl11pt = '           .'

    call jemarq()

    call pgpget(sd_pgp,'RESU_OUT',kscal=result)
    call pgpget(sd_pgp,'NB_OBSER',iscal=nbobs)

    call jeveuo(result//'           .TBLP', 'L', jtblp)
    nomjv = zk24(jtblp+4*(9-1)+2)
    call jeveuo(nomjv,'E',jvecr)
    nomjv = zk24(jtblp+4*(10-1)+2)
    call jeveuo(nomjv,'E',jvecc)


    call pgpget(sd_pgp,'RESU_IN ',kscal=resin)
    call pgpget(sd_pgp,'TYP_RESU ',kscal=typres)
    call pgpget(sd_pgp, 'BASE',kscal=base)
    call dismoi('NB_MODES_TOT', base, 'RESULTAT', repi=nbmodes)

!   Line counter, across the whole table (for different observations)
    lc = 0
    do iobs = 1,nbobs
        call pgpget(sd_pgp,'NOM_CHAM ',iobs=iobs, kscal=champ)
        call pgpget(sd_pgp,'TYP_CHAM ',iobs=iobs, kscal=typcha)
!
        chreco = 'DEPL'
        if ((champ(1:4).eq.'VITE').or.(champ(1:4).eq.'ACCE')) chreco=champ(1:4)
!
        call pgpget(sd_pgp,'NUM_ORDR',iobs=iobs, lonvec=nord)
        AS_ALLOCATE(vi=lordr , size=nord)
        call pgpget(sd_pgp, 'NUM_ORDR', iobs=iobs, ivect = lordr)
!
        call jeveuo(resin//bl11pt//chreco,'L',jevol)

        call pgpget(sd_pgp,'REF_COMP',iobs=iobs, lonvec=physlen)
        call pgpget(sd_pgp,'TYP_SCAL',iobs=iobs, kscal=typsc)

        if (typsc(1:1).eq.'R') then

            call pgpget(sd_pgp,'VEC_PR_R ',iobs=iobs, lonvec=length)
            AS_ALLOCATE(vr=vectr , size=length)
            call pgpget(sd_pgp,'VEC_PR_R ',iobs=iobs, rvect=vectr)
        
            if (typres(1:4).eq.'TRAN') then
                do iord = 1, nord
                    dec1 = lc + (iord-1)*physlen

                    do i = 1, physlen
                        physvalr = 0.d0
                        do j = 1, nbmodes
                            physvalr = physvalr + vr(i,j)*evolr(j,lordr(iord))
                        end do
                        zr(jvecr+dec1+i-1) = physvalr
                    end do
                end do

            else if (typres(1:4).eq.'HARM') then
                do iord = 1, nord
                    dec1 = lc + (iord-1)*physlen
                    do i = 1, physlen
                        physvalc = dcmplx(0.d0,0.d0)
                        do j = 1, nbmodes
                           physvalc = physvalc + dcmplx(vr(i,j),0.d0)*evolc(j,lordr(iord))
                        end do
                        zc(jvecc+dec1+i-1) = physvalc
                    end do
                end do

            end if
            AS_DEALLOCATE(vr=vectr)

        else if (typsc(1:1).eq.'C') then

            call pgpget(sd_pgp,'VEC_PR_C ',iobs=iobs, lonvec=length)
            AS_ALLOCATE(vc=vectc , size=length)
            call pgpget(sd_pgp,'VEC_PR_C ',iobs=iobs, cvect=vectc)

            if (typres(1:4).eq.'TRAN') then
                do iord = 1, nord
                    dec1 = lc + (iord-1)*physlen
                    do i = 1, physlen
                        physvalc = dcmplx(0.d0,0.d0)
                        do j = 1, nbmodes
                            physvalc = physvalc + vc(i,j)*dcmplx(evolr(j,lordr(iord)),0.d0)
                        end do
                        zc(jvecc+dec1+i-1) = physvalc
                    end do
                end do

            else if (typres(1:4).eq.'HARM') then
                do iord = 1, nord
                    dec1 = lc + (iord-1)*physlen
                    do i = 1, physlen
                        physvalc = dcmplx(0.d0,0.d0)
                        do j = 1, nbmodes
                            physvalc = physvalc + vc(i,j)*evolc(j,lordr(iord))
                        end do
                        zc(jvecc+dec1+i-1) = physvalc
                    end do
                end do
            end if
            AS_DEALLOCATE(vc=vectc)
        end if
!
        lc = lc + nord*physlen
        call jelibe(resin//bl11pt//chreco)
        AS_DEALLOCATE(vi=lordr)
!
    end do

    call jedema()

end subroutine
