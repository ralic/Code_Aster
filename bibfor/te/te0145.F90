subroutine te0145(option, nomte)
! aslint: disable=
    implicit none
#include "jeveux.h"
#include "asterfort/jevech.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
!
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
!     CALCUL FORCES ELEMENTAIRES ELECTRIQUES POUR LES ELEMENTS DE POUTRE
!     ------------------------------------------------------------------
! IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
!        'CHAR_MECA' : CALCUL DE LA MATRICE DE MASSE
! IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
!        'MECA_POU_D_E' : POUTRE DROITE D'EULER       (SECTION VARIABLE)
!        'MECA_POU_D_T' : POUTRE DROITE DE TIMOSHENKO (SECTION VARIABLE)
!        'MECA_POU_C_T' : POUTRE COURBE DE TIMOSHENKO(SECTION CONSTANTE)
!     ------------------------------------------------------------------
!
!
!
    character(len=8) :: nomail
    real(kind=8) :: zero, deux, zcod
    real(kind=8) :: xl, e1, e2, e3, f1, f2, f3, r1, r2, r3, q1, q2, q3
    real(kind=8) :: b1, b2, b3, u(3), s, alp, d, um, an1, an2, an3, an4
    real(kind=8) :: v1, v2, v3, v(3), w(3), wm, pf1, pf2
    real(kind=8) :: rad, ang, angs2
    real(kind=8) :: force(12)
    integer :: ipt
    integer :: iadzi, iazk24
!-----------------------------------------------------------------------
    integer :: i, iforc, ivect, j, lrcou, lx
    real(kind=8) :: dd
!-----------------------------------------------------------------------
    zero = 0.d0
    deux = 2.d0
!     ------------------------------------------------------------------
!
!
!     --- INITIALISATION
!
    do 40 i = 1, 12
        force(i) = zero
40  end do
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
    if (nomte .eq. 'MECA_POU_C_T') then
        call utmess('F', 'ELEMENTS3_29')
        call jevech('PCAARPO', 'L', lrcou)
        rad = zr(lrcou)
        angs2 = asin( xl / ( deux * rad ) )
        ang = angs2 * deux
        xl = rad * ang
    endif
!
!     ------------------- CALCUL DES VECTEURS ELEMENTAIRES ------------
!
    call jevech('PFRELEC', 'L', iforc)
    zcod=zr(iforc+6)
    call jevech('PVECTUR', 'E', ivect)
!C    BARRES INFINIES PARALLELES MULTIPLES (CAS VIBRAPORTIC)
    if (zcod .eq. 10.d0) then
        u(1)=(zr(lx+4)-zr(lx+1))/xl
        u(2)=(zr(lx+5)-zr(lx+2))/xl
        u(3)=(zr(lx+6)-zr(lx+3))/xl
        w(1)=u(2)*zr(iforc-1+3)-u(3)*zr(iforc-1+2)
        w(2)=u(3)*zr(iforc-1+1)-u(1)*zr(iforc-1+3)
        w(3)=u(1)*zr(iforc-1+2)-u(2)*zr(iforc-1+1)
        wm=sqrt(w(1)**2+w(2)**2+w(3)**2)
        if (wm .lt. 1.d-3) then
            do 10 j = 1, 3
                force(j)=force(j)+xl*zr(iforc-1+j)/deux
                force(j+6)=force(j+6)+xl*zr(iforc-1+j)/deux
10          continue
            goto 1000
        endif
        w(1)=w(1)/wm
        w(2)=w(2)/wm
        w(3)=w(3)/wm
        v(1)=w(2)*u(3)-w(3)*u(2)
        v(2)=w(3)*u(1)-w(1)*u(3)
        v(3)=w(1)*u(2)-w(2)*u(1)
        pf1=zr(iforc)*u(1)+zr(iforc+1)*u(2)+zr(iforc+2)*u(3)
        pf2=zr(iforc)*v(1)+zr(iforc+1)*v(2)+zr(iforc+2)*v(3)
        do 20 j = 1, 3
            force(j)=force(j)+xl*(pf1*u(j)+pf2*v(j))/2.d0
            force(j+3)=force(j+3)+pf2*w(j)*xl**2/12.d0
            force(j+6)=force(j+6)+xl*(pf1*u(j)+pf2*v(j))/2.d0
            force(j+9)=force(j+9)-pf2*w(j)*xl**2/12.d0
20      continue
!
        goto 1000
    endif
!
    e1=zr(lx+4)-zr(lx+1)
    e2=zr(lx+5)-zr(lx+2)
    e3=zr(lx+6)-zr(lx+3)
    s=sqrt(e1**2+e2**2+e3**2)
    e1=e1/s
    e2=e2/s
    e3=e3/s
!C    2 BARRES INFINIES PARALLELES DEFINIES PAR UNE TRANSLATION
    if (zcod .eq. 11.d0) then
        r1=-zr(iforc)
        r2=-zr(iforc+1)
        r3=-zr(iforc+2)
        b1=e2*r3-e3*r2
        b2=e3*r1-e1*r3
        b3=e1*r2-e2*r1
        d=sqrt(b1**2+b2**2+b3**2)
        u(1)=e2*b3-e3*b2
        u(2)=e3*b1-e1*b3
        u(3)=e1*b2-e2*b1
        s=sqrt(u(1)**2+u(2)**2+u(3)**2)
        u(1)=u(1)/s
        u(2)=u(2)/s
        u(3)=u(3)/s
        v1=u(1)
        v2=u(2)
        v3=u(3)
        w(1)=e2*v3-e3*v2
        w(2)=e3*v1-e1*v3
        w(3)=e1*v2-e2*v1
        do 11 j = 1, 3
            force(j)=force(j)+xl*u(j)/d/deux
            force(j+3)=force(j+3)+xl**2*w(j)/d/12.d0
            force(j+6)=force(j+6)+xl*u(j)/d/deux
            force(j+9)=force(j+9)-xl**2*w(j)/d/12.d0
11      continue
        goto 1000
    endif
!C    2 BARRES INFINIES PARALLELES DEFINIES PAR UNE DISTANCE ET 1 POINT
    if (zcod .eq. 12.d0) then
        d=zr(iforc)
        r1=zr(lx+4)/2.d0+zr(lx+1)/2.d0-zr(iforc+3)
        r2=zr(lx+5)/2.d0+zr(lx+2)/2.d0-zr(iforc+4)
        r3=zr(lx+6)/2.d0+zr(lx+3)/2.d0-zr(iforc+5)
        b1=e2*r3-e3*r2
        b2=e3*r1-e1*r3
        b3=e1*r2-e2*r1
        u(1)=e2*b3-e3*b2
        u(2)=e3*b1-e1*b3
        u(3)=e1*b2-e2*b1
        s=sqrt(u(1)**2+u(2)**2+u(3)**2)
        u(1)=u(1)/s
        u(2)=u(2)/s
        u(3)=u(3)/s
        v1=u(1)
        v2=u(2)
        v3=u(3)
        w(1)=e2*v3-e3*v2
        w(2)=e3*v1-e1*v3
        w(3)=e1*v2-e2*v1
        do 12 j = 1, 3
            force(j)=force(j)+xl*u(j)/d/deux
            force(j+3)=force(j+3)+xl**2*w(j)/d/12.d0
            force(j+6)=force(j+6)+xl*u(j)/d/deux
            force(j+9)=force(j+9)-xl**2*w(j)/d/12.d0
12      continue
        goto 1000
    endif
!C    2 BARRES EN POSITION QUELCONQUE
    if (zcod .eq. 2.d0 .or. zcod .eq. 3.d0) then
        do 30 i = 1, 100
            alp=(dble(i)-0.5d0)/100.d0
            an1=1.d0-3.d0*alp**2+2.d0*alp**3
            an2=(alp-2.d0*alp**2+alp**3)*xl
            an3=3.d0*alp**2-2.d0*alp**3
            an4=(-alp**2+alp**3)*xl
            r1=zr(lx+4)*(1.d0-alp)+zr(lx+1)*alp-zr(iforc+3)
            r2=zr(lx+5)*(1.d0-alp)+zr(lx+2)*alp-zr(iforc+4)
            r3=zr(lx+6)*(1.d0-alp)+zr(lx+3)*alp-zr(iforc+5)
            q1=zr(lx+4)*(1.d0-alp)+zr(lx+1)*alp-zr(iforc)
            q2=zr(lx+5)*(1.d0-alp)+zr(lx+2)*alp-zr(iforc+1)
            q3=zr(lx+6)*(1.d0-alp)+zr(lx+3)*alp-zr(iforc+2)
            f1=zr(iforc+3)-zr(iforc)
            f2=zr(iforc+4)-zr(iforc+1)
            f3=zr(iforc+5)-zr(iforc+2)
            s=sqrt(f1**2+f2**2+f3**2)
            f1=f1/s
            f2=f2/s
            f3=f3/s
            b1=f2*q3-f3*q2
            b2=f3*q1-f1*q3
            b3=f1*q2-f2*q1
            d=sqrt(b1**2+b2**2+b3**2)
            dd=d/sqrt(q1**2+q2**2+q3**2)
            if (dd .lt. 1.d-8) goto 30
            b1=b1/d
            b2=b2/d
            b3=b3/d
            u(1)=e2*b3-e3*b2
            u(2)=e3*b1-e1*b3
            u(3)=e1*b2-e2*b1
            um=sqrt(u(1)**2+u(2)**2+u(3)**2)
            v1=u(1)/um
            v2=u(2)/um
            v3=u(3)/um
            w(1)=e2*v3-e3*v2
            w(2)=e3*v1-e1*v3
            w(3)=e1*v2-e2*v1
!C    2 BARRES EN POSITION QUELCONQUE INFINIES
            if (zcod .eq. 2.d0) s=0.01d0*xl/d
!C    2 BARRES EN POSITION QUELCONQUE FINIES
            if (zcod .eq. 3.d0) then
                s=sqrt(q1**2+q2**2+q3**2)
                q1=q1/s
                q2=q2/s
                q3=q3/s
                s=sqrt(r1**2+r2**2+r3**2)
                r1=r1/s
                r2=r2/s
                r3=r3/s
                s=f1*(q1-r1)+f2*(q2-r2)+f3*(q3-r3)
                s=s*5.d-3*xl/d
            endif
            do 2 j = 1, 3
                force(j)=force(j)+s*u(j)*an1
                force(j+3)=force(j+3)+s*w(j)*an2
                force(j+6)=force(j+6)+s*u(j)*an3
                force(j+9)=force(j+9)+s*w(j)*an4
 2          continue
30      continue
    endif
!
1000  continue
!
!   --- STOCKAGE
!
    ipt = 6
    if (nomte .eq. 'MECA_POU_D_EM' .or. nomte .eq. 'MECA_POU_D_TG' .or. nomte .eq.&
        'MECA_POU_D_TGM') ipt=7
!
    do 50 i = 1, 6
        zr(ivect-1+i) = force(i)
        zr(ivect-1+i+ipt)= force(i+6)
50  end do
end subroutine
