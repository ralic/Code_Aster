subroutine te0531(option,nomte)
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
!
    implicit none
!
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/verift.h"
#include "asterfort/tecach.h"
#include "asterfort/jevech.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lteatt.h"
#include "asterfort/pmfinfo.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvarc.h"
#include "asterfort/rcvalb.h"
#include "asterfort/utmess.h"
!
    character(len=16)  :: nomte,option
! ======================================================================
!
!    OPTIONS  : 'EPVC_ELGA' 'EPME_ELGA' 'EPSP_ELGA'
!    ELEMENTS : DKT, GRILLE, PMF, TUYAU, BARRE
!
!    POUR ELVC_ELGA :
!    1 COMPOSANTES :
!    EPTHER= DILATATION THERMIQUE (LONGI)   : ALPHA*(T-TREF)
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!              ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................

    integer :: npg
    integer :: nbcmp, jnbspi, idefo, nbcou, icomp, imate, iret
    integer :: nbsp, ipg, ksp, i, idefto, icomp2, idsig, icomp3
    integer :: nbgf, icp, ngf, isdcom, icompo, ig, nbfig, ifib, icaba
    integer :: nbsec, iret1, nbpar, nbv, icodre(2), tygrfi, nbcarm, nug(10)
    real(kind=8) :: epsth, sigma(6), trsig, temp, valres(2), e ,nu, c1, c2, a
    character(len=4) :: fami
    character(len=8) :: materi, nompar, nomres(2)
    character(len=32) :: phenom
    aster_logical :: lmeca, pmf, grille, tuyau, barre, dkt, lplas

    nbcmp=1
    materi = ' '
    lmeca = .false.
    lplas = .false.
    fami = 'RIGI'
!
    call elrefe_info(fami=fami, npg=npg)
    
    grille = lteatt('GRILLE' ,'OUI')
    pmf    = lteatt('TYPMOD2','PMF')
    tuyau  = lteatt('TUYAU'  ,'OUI')
    barre  = (nomte.eq.'MECA_BARRE')
    dkt    = (nomte(1:4).eq.'MEDK')
!
    call tecach('NNN', 'PMATERC', 'L', iret, iad=imate)
    call jevech('PDEFOPG', 'E', idefo)
                
    if (option .eq. 'EPME_ELGA' .or. option .eq. 'EPSP_ELGA')then
        lmeca = .true.
        call jevech('PDEFORR', 'L', idefto)
    endif
!
    if (option .eq. 'EPSP_ELGA')then
        lplas = .true.
        call jevech('PCONTRR', 'L', idsig)
!
        nbv = 2
        nomres(1) = 'E'
        nomres(2) = 'NU'
        nbpar = 1
        nompar = 'TEMP'
!
        call rccoma(zi(imate), 'ELAS', 1, phenom, icodre(1))
        if (phenom .ne. 'ELAS') call utmess('F','ELEMENTS_49', valk = [phenom, option])
    endif
!
    if (.not. grille .and. .not. barre)then
        call jevech('PNBSP_I', 'L', jnbspi)
    else
        nbsp = 1
    endif
!
    if (dkt .or. tuyau)then
!
        nbcou=zi(jnbspi-1+1)
        if (lplas) then
            call utmess('A', 'ELEMENTS3_13')
        endif
        if (tuyau) then
            nbsec = zi(jnbspi-1+2)
        else
            nbsec = 3
        endif
        nbsp = nbcou*nbsec
        if (lmeca) nbcmp=6
!
        do ipg = 1,npg
            do ksp = 1,nbsp
!
                call verift(fami, ipg, ksp, '+', zi( imate),&
                            epsth=epsth)
!
                icomp = idefo+nbcmp*nbsp*(ipg-1)+nbcmp*(ksp-1)-1
                if (lmeca) then
                    icomp2 = idefto+nbcmp*nbsp*(ipg-1)+nbcmp*(ksp-1)-1
                    do  i = 1, 2
                        zr(icomp + i) = zr(icomp2 + i) - epsth
                    enddo
                    do  i = 3, 6
                        zr(icomp + i) = zr(icomp2 + i)
                    enddo
                    if (lplas)then
                        call rcvarc(' ', 'TEMP', '+', fami, ipg,&
                                    ksp, temp, iret1)
                        call rcvalb(fami, ipg, ksp, '+', zi(imate),&
                                    ' ', phenom, nbpar, nompar, [temp],&
                                    nbv, nomres, valres, icodre, 1)
!
                        e = valres(1)
                        nu = valres(2)
                        c1 = (1.d0+nu)/e
                        c2 = nu/e
!
                        icomp3 = idsig+nbcmp*nbsp*(ipg-1)+nbcmp*(ksp-1)-1
!
                        do i = 1, nbcmp
                            sigma(i) = zr(icomp3 + i)
                        enddo
!
                        trsig = sigma(1) + sigma(2) + sigma(3)
!
!                       soustraction des deformations elastiques
                        do  i = 1, 2
                            zr(icomp + i) = zr(icomp + i) - (c1* sigma(i)-c2*trsig)
                        enddo
!                       on laisse la composante 3 nulle
                        zr(icomp + 4) = zr(icomp + 4) - c1* sigma(4)
!                       les composantes EPXZ et EPYZ mises à zero,
!                       le calcul produirait des resultats faux car
!                       SIXZ et SIYZ sont nulles dans le champ de contrainte
                        do  i = 5, 6
                            zr(icomp + i) = 0.D0
                        enddo
                    endif
                else
                    zr(icomp + 1) = epsth
                endif
!
            enddo
        enddo
!
    elseif (grille .or. barre)then

        if (barre .and. lplas) then
            call jevech('PCAGNBA', 'L', icaba)
            a = zr(icaba)
        else
            a = 1.d0
        endif
!
        do ipg = 1,npg
            do ksp = 1,nbsp
!
                call verift(fami, ipg, ksp, '+', zi( imate),&
                            epsth=epsth)
!
                icomp = idefo+nbcmp*nbsp*(ipg-1)+nbcmp*(ksp-1)-1
                if (lmeca) then
                    icomp2 = idefto+nbcmp*nbsp*(ipg-1)+nbcmp*(ksp-1)-1
                    zr(icomp + 1) = zr(icomp2 + 1) - epsth
                    if (lplas)then
                        call rcvarc(' ', 'TEMP', '+', fami, ipg,&
                                    ksp, temp, iret1)
                        call rcvalb(fami, ipg, ksp, '+', zi(imate),&
                                    ' ', phenom, nbpar, nompar, [temp],&
                                    nbv, nomres, valres, icodre, 1)
!
                        e = valres(1)
                        c1 = 1.d0/e
!
                        icomp3 = idsig+nbcmp*nbsp*(ipg-1)+nbcmp*(ksp-1)-1
!                       soustraction des deformations elastiques
                        zr(icomp + 1) = zr(icomp + 1) - c1* zr(icomp3 + 1)/a
                    endif
                else
                    zr(icomp + 1) = epsth
                endif
!
            enddo
        enddo
!
    elseif (pmf)then
!       Récupération des caractéristiques des fibres
        call pmfinfo(nbsp, nbgf, tygrfi, nbcarm, nug)
!
        call jevech('PCOMPOR', 'L', icompo)
        call jeveuo(zk16(icompo-1+7), 'L', isdcom) 
        do ipg = 1,npg
            ksp = 0
            do ig = 1, nbgf
                ngf = nug(ig)
                icp = (ngf-1)*6
!               nombre de fibres de ce groupe
                read(zk24(isdcom-1+icp+6),'(I24)') nbfig
                materi = zk24(isdcom-1+icp+2)(1:8)
                do ifib = 1,nbfig
!
                    ksp = ksp + 1
                    ASSERT(ksp.le.nbsp)
                                
                    call verift(fami, ipg, ksp, '+', zi( imate),&
                                materi_=materi, epsth=epsth)

                    icomp = idefo+nbcmp*nbsp*(ipg-1)+nbcmp*(ksp-1)-1
                    if (lmeca) then
                        icomp2 = idefto+nbcmp*nbsp*(ipg-1)+nbcmp*(ksp-1)-1
                        zr(icomp + 1) = zr(icomp2 + 1) - epsth
                        if (lplas)then
                            call rcvarc(' ', 'TEMP', '+', fami, ipg,&
                                        ksp, temp, iret1)
                            call rcvalb(fami, ipg, ksp, '+', zi(imate),&
                                        ' ', phenom, nbpar, nompar, [temp],&
                                        nbv, nomres, valres, icodre, 1)
!
                            e = valres(1)
                            c1 = 1.d0/e
!
                            icomp3 = idsig+nbcmp*nbsp*(ipg-1)+nbcmp*(ksp-1)-1
!
                            do i = 1, nbcmp
                                sigma(i) = zr(icomp3 + i)
                            enddo
!                           soustraction des deformations elastiques
                            zr(icomp + 1) = zr(icomp + 1) - c1* zr(icomp3 + 1)
                        endif
                    else
                        zr(icomp + 1) = epsth
                    endif
!
                enddo
            enddo
        enddo
    else
        ASSERT(.false.)
    endif
!
end subroutine
