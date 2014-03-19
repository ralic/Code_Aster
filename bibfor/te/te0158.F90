subroutine te0158(option, nomte)
    implicit none
#include "jeveux.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/jpd1ff.h"
#include "asterfort/jsd1ff.h"
#include "asterfort/matela.h"
#include "asterfort/matrot.h"
#include "asterfort/moytem.h"
#include "asterfort/pmfdge.h"
#include "asterfort/pmfpti.h"
#include "asterfort/tecach.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
#include "asterfort/utpvgl.h"
#include "asterfort/verifm.h"
    character(len=*) :: option, nomte
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
! IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
!      'DEGE_ELNO     : DEFORMATIONS GENERALISEES DE POUTRE
! IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
!       'MECA_POU_D_E'  : POUTRE DROITE D'EULER       (SECTION VARIABLE)
!       'MECA_POU_D_T'  : POUTRE DROITE DE TIMOSHENKO (SECTION VARIABLE)
!       'MECA_POU_C_T'  : POUTRE COURBE DE TIMOSHENKO(SECTION CONSTANTE)
!       'MECA_POU_D_EM' : POUTRE DROITE MULTIFIBRE D EULER (SECT. CONST)
!       'MECA_POU_D_TG' : POUTRE DROITE DE TIMOSHENKO (GAUCHISSEMENT)
!       'MECA_POU_D_TGM': POUTRE DROITE DE TIMOSHENKO (GAUCHISSEMENT)
!                         MULTI-FIBRES SECTION CONSTANTE
!     ------------------------------------------------------------------
! IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
! IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
!     ------------------------------------------------------------------
!
    integer :: jeffg, lmater, lsect, lx, iret, lorien, jdepl, i, j, kp, nc
    integer :: itemp, jtab(7), istrxr
!
    character(len=4) :: fami
    character(len=8) :: nomail
    character(len=16) :: ch16
    integer :: lsect2, ipos, in, iadzi, iazk24
    integer :: npg, ndim, nno, nnos, ipoids, ivf
    real(kind=8) :: b(4), gg, xi, wi
    real(kind=8) :: ul(14), pgl(3, 3), d1b(6, 12), dege(3, 7), d1btg(7, 14)
    real(kind=8) :: degem(6), alpha
    real(kind=8) :: zero, un, deux, temp, e, xnu, epsthe(1), g, xl
    real(kind=8) :: a, xiy, xiz, alfay, alfaz, phiy, phiz
    real(kind=8) :: ksi1, d1b3(2, 3), ey, ez
!     ------------------------------------------------------------------
    zero = 0.d0
    un = 1.d0
    deux = 2.d0
!     ------------------------------------------------------------------
!
    fami = 'RIGI'
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf)
!
    if (option .eq. 'DEGE_ELNO') then
        call jevech('PDEFOGR', 'E', jeffg)
    else
        ch16 = option
        call utmess('F', 'ELEMENTS2_47', sk=ch16)
    endif
!
!     --- RECUPERATION DES COORDONNEES DES NOEUDS ---
    call jevech('PGEOMER', 'L', lx)
    lx = lx - 1
    xl = sqrt( (zr(lx+4)-zr(lx+1))**2 + (zr(lx+5)-zr(lx+2))**2 + (zr(lx+6)-zr(lx+3) )**2 )
    if (xl .eq. zero) then
        call tecael(iadzi, iazk24)
        nomail = zk24(iazk24-1+3)(1:8)
        call utmess('F', 'ELEMENTS2_43', sk=nomail)
    endif
!
!     --- RECUPERATION DES ORIENTATIONS ---
    call jevech('PCAORIE', 'L', lorien)
    call matrot(zr(lorien), pgl)
!
!     --- RECUPERATION DES DEPLACEMENTS ---
    call jevech('PDEPLAR', 'L', jdepl)
!
    if (nomte .ne. 'MECA_POU_D_EM') then
!     --- CARACTERISTIQUES MATERIAUX ---
        call jevech('PMATERC', 'L', lmater)
!
        call verifm(fami, npg, 1, '+', zi(lmater),&
                    'ELAS', 1, epsthe, iret)
        itemp=0
        if (iret .eq. 0) itemp=1
!
        call moytem(fami, npg, 1, '+', temp,&
                    iret)
        call matela(zi(lmater), ' ', itemp, temp, e,&
                    xnu)
!
        g = e / ( deux * ( un + xnu ) )
!
!        --- CARACTERISTIQUES GENERALES DES SECTIONS ---
        call jevech('PCAGNPO', 'L', lsect)
        lsect = lsect-1
!
!        --- SECTION INITIALE ---
        a = zr(lsect+ 1)
        xiy = zr(lsect+ 2)
        xiz = zr(lsect+ 3)
        alfay = zr(lsect+ 4)
        alfaz = zr(lsect+ 5)
!
        if (nomte .eq. 'MECA_POU_D_TG' .or. nomte .eq. 'MECA_POU_D_TGM') then
            lsect2 = lsect + 11
            ey = -(zr(lsect+6)+zr(lsect2+6))/deux
            ez = -(zr(lsect+7)+zr(lsect2+7))/deux
        endif
!
        if (nomte .eq. 'MECA_POU_D_E') then
            nc = 6
            phiy = zero
            phiz = zero
        else if (nomte .eq. 'MECA_POU_D_T') then
            nc = 6
            phiy = e*xiz*12.d0*alfay/ (xl*xl*g*a)
            phiz = e*xiy*12.d0*alfaz/ (xl*xl*g*a)
            elseif (nomte.eq.'MECA_POU_D_TG' .or.&
     &           nomte.eq.'MECA_POU_D_TGM' ) then
            nc = 7
            phiy = e*xiz*12.d0*alfay/ (xl*xl*g*a)
            phiz = e*xiy*12.d0*alfaz/ (xl*xl*g*a)
        else
            ch16 = nomte
            call utmess('F', 'ELEMENTS2_42', sk=ch16)
        endif
!
!        --- PASSAGE DES DEPLACEMENTS DANS LE REPERE LOCAL ---
        call utpvgl(nno, nc, pgl, zr(jdepl), ul)
!
        if (nomte .eq. 'MECA_POU_D_TG' .or. nomte .eq. 'MECA_POU_D_TGM') then
!
!        --- PASSAGE DE G (CENTRE DE GRAVITE) A C (CENTRE DE TORSION)
            do i = 1, 2
                ul(7* (i-1)+2) = ul(7* (i-1)+2) - ez*ul(7* (i-1)+4)
                ul(7* (i-1)+3) = ul(7* (i-1)+3) + ey*ul(7* (i-1)+4)
            enddo
        endif
!
!        BOUCLE SUR LES POINTS DE GAUSS
        do kp = 1, 3
            if (nomte .eq. 'MECA_POU_D_TG' .or. nomte .eq. 'MECA_POU_D_TGM') then
                call jsd1ff(kp, xl, phiy, phiz, d1btg)
            else
                call jpd1ff(kp, xl, phiy, phiz, d1b)
            endif
!
            do i = 1, nc
                dege(kp,i) = zero
                do j = 1, 2*nc
                    if (nomte .eq. 'MECA_POU_D_TG' .or. nomte .eq. 'MECA_POU_D_TGM') then
                        dege(kp,i) = dege(kp,i) + d1btg(i,j)*ul(j)
                    else
                        dege(kp,i) = dege(kp,i) + d1b(i,j)*ul(j)
                    endif
                enddo
            enddo
            dege(kp,1) = dege(kp,1) - epsthe(1)
!
        enddo
!

!        --- POUR LE POINT 1 ---
        ksi1 = -sqrt( 5.d0 / 3.d0 )
        d1b3(1,1) = ksi1*(ksi1-1.d0)/2.0d0
        d1b3(1,2) = 1.d0-ksi1*ksi1
        d1b3(1,3) = ksi1*(ksi1+1.d0)/2.0d0
!        --- POUR LE POINT 2 ---
        ksi1 = sqrt( 5.d0 / 3.d0 )
        d1b3(2,1) = ksi1*(ksi1-1.d0)/2.0d0
        d1b3(2,2) = 1.d0-ksi1*ksi1
        d1b3(2,3) = ksi1*(ksi1+1.d0)/2.0d0
!
        do i = 1, nc
            do kp = 1, 3
                zr(jeffg+i-1) =zr(jeffg+i-1) +dege(kp,i)*d1b3(1,&
                kp)
                zr(jeffg+nc+i-1)=zr(jeffg+nc+i-1)+dege(kp,i)*d1b3(&
                2,kp)
            enddo
        enddo
    else
!
!     POUTRE MULTIFIBRES MECA_POU_D_EM
        nc = 6
!        --- PASSAGE DES DEPLACEMENTS DANS LE REPERE LOCAL ---
        call utpvgl(nno, nc, pgl, zr(jdepl), ul)
!       alpha modes incompatibles
        call tecach('ONN','PSTRXRR','L',iret, nval=7,&
                    itab= jtab)
        istrxr=jtab(1)
        alpha=zr(istrxr-1+15)
!
        do in = 1, 2
            call pmfpti(-in, zr(ipoids), zr(ivf), xl, xi,&
                        wi, b, gg)
            call pmfdge(b, gg, ul, alpha, degem)
            ipos=jeffg+nc*(in-1)
            do i = 1, nc
                zr(ipos+i-1) = degem(i)
            enddo
        enddo
    endif
!
end subroutine
