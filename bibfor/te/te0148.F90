subroutine te0148(option, nomte)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jevech.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/tecael.h'
    include 'asterfort/u2mesk.h'
    character(len=*) :: option, nomte
!     ------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!     CALCUL FORCES ELEMENTAIRES DE LAPLACE
!     ------------------------------------------------------------------
! IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
!        'CHAR_MECA' : CALCUL DE LA FORCE DE LAPLACE
! IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
!        'MECA_POU_D_T' : POUTRE DROITE DE TYMOSHENKO (SECTION VARIABLE)
!        'MECA_POU_D_E' : POUTRE DROITE D  EULER      (SECTION VARIABLE)
!     ------------------------------------------------------------------
!
!
!
    character(len=8) :: nomail
    character(len=16) :: listma, ltrans
    character(len=19) :: chgeom
    real(kind=8) :: zero
    real(kind=8) :: xl, e1, e2, e3, f1, f2, f3, r1, r2, r3, q1, q2, q3, dd
    real(kind=8) :: b1, b2, b3, u(3), s, d, um
    real(kind=8) :: alp(20), an1(20), an2(20), an3(20), an4(20)
    real(kind=8) :: v1, v2, v3, w(3)
    real(kind=8) :: x0, y0, z0, x1, y1, z1, x2, y2, z2, a, b, c, lam1, lam2
    real(kind=8) :: force(12)
    integer :: ipt
    character(len=1) :: k1bid
    integer :: iadzi, iazk24
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, ilapl, ilist, ima, ivect, j, jgeom
    integer :: jlima, jtran, lx, nbma, nbma2, no1, no2
!
!-----------------------------------------------------------------------
    call jemarq()
    zero = 0.d0
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
        call u2mesk('F', 'ELEMENTS2_43', 1, nomail)
    endif
!
!     ------------------- CALCUL DES VECTEURS ELEMENTAIRES ------------
!
    call jevech('PFLAPLA', 'L', ilapl)
    call jevech('PLISTMA', 'L', ilist)
    call jevech('PVECTUR', 'E', ivect)
    listma=zk16(ilist)
    ltrans=zk16(ilist+1)
    chgeom=zk24(ilapl+1)(1:19)
    call jeveuo(chgeom//'.VALE', 'L', jgeom)
!
    e1=zr(lx+4)-zr(lx+1)
    e2=zr(lx+5)-zr(lx+2)
    e3=zr(lx+6)-zr(lx+3)
    s=sqrt(e1**2+e2**2+e3**2)
    e1=e1/s
    e2=e2/s
    e3=e3/s
    if (listma .eq. ' ' .or. ltrans .eq. ' ') goto 1000
    call jeveuo(listma, 'L', jlima)
    call jelira(listma, 'LONMAX', nbma2, k1bid)
    nbma=nbma2/2
    call jeveuo(ltrans, 'L', jtran)
    x0=zr(jtran)
    y0=zr(jtran+1)
    z0=zr(jtran+2)
    a=zr(jtran+3)
    b=zr(jtran+4)
    c=zr(jtran+5)
!C    2 BARRES EN POSITION QUELCONQUE
    do 1 ima = 1, nbma
        no1=zi(jlima+2*ima-2)
        no2=zi(jlima+2*ima-1)
        if (a .ne. 0.0d0 .or. b .ne. 0.0d0 .or. c .ne. 0.0d0) then
            x1=zr(jgeom+3*no1-3)
            y1=zr(jgeom+3*no1-2)
            z1=zr(jgeom+3*no1-1)
            x2=zr(jgeom+3*no2-3)
            y2=zr(jgeom+3*no2-2)
            z2=zr(jgeom+3*no2-1)
            lam1=(a*(x1-x0)+b*(y1-y0)+c*(z1-z0))/(a**2+b**2+c**2)
            lam2=(a*(x2-x0)+b*(y2-y0)+c*(z2-z0))/(a**2+b**2+c**2)
            f1=x2-x1-2.d0*(lam2-lam1)*a
            f2=y2-y1-2.d0*(lam2-lam1)*b
            f3=z2-z1-2.d0*(lam2-lam1)*c
            s=sqrt(f1**2+f2**2+f3**2)
            f1=f1/s
            f2=f2/s
            f3=f3/s
        else
            f1=zr(jgeom+3*no2-3)-zr(jgeom+3*no1-3)
            f2=zr(jgeom+3*no2-2)-zr(jgeom+3*no1-2)
            f3=zr(jgeom+3*no2-1)-zr(jgeom+3*no1-1)
            s=sqrt(f1**2+f2**2+f3**2)
            f1=f1/s
            f2=f2/s
            f3=f3/s
        endif
        do 30 i = 1, 5
            if (ima .gt. 1) goto 31
            alp(i)=(dble(i)-0.5d0)/5.d0
            an1(i)=1.d0-3.d0*alp(i)**2+2.d0*alp(i)**3
            an2(i)=(alp(i)-2.d0*alp(i)**2+alp(i)**3)*xl
            an3(i)=3.d0*alp(i)**2-2.d0*alp(i)**3
            an4(i)=(-alp(i)**2+alp(i)**3)*xl
31          continue
            if (a .eq. 0.0d0 .and. b .eq. 0.0d0 .and. c .eq. 0.0d0) then
!C           CAS DES TRANSLATIONS
                r1=zr(lx+4)*(1.d0-alp(i))+zr(lx+1)*alp(i)-zr(jgeom+3*&
                no2-3) -zr(jtran)
                r2=zr(lx+5)*(1.d0-alp(i))+zr(lx+2)*alp(i)-zr(jgeom+3*&
                no2-2) -zr(jtran+1)
                r3=zr(lx+6)*(1.d0-alp(i))+zr(lx+3)*alp(i)-zr(jgeom+3*&
                no2-1) -zr(jtran+2)
                q1=zr(lx+4)*(1.d0-alp(i))+zr(lx+1)*alp(i)-zr(jgeom+3*&
                no1-3) -zr(jtran)
                q2=zr(lx+5)*(1.d0-alp(i))+zr(lx+2)*alp(i)-zr(jgeom+3*&
                no1-2) -zr(jtran+1)
                q3=zr(lx+6)*(1.d0-alp(i))+zr(lx+3)*alp(i)-zr(jgeom+3*&
                no1-1) -zr(jtran+2)
            else
!C           CAS DES SYMETRIES
                r1=zr(lx+4)*(1.d0-alp(i))+zr(lx+1)*alp(i)-x2+2.d0*&
                lam2*a
                r2=zr(lx+5)*(1.d0-alp(i))+zr(lx+2)*alp(i)-y2+2.d0*&
                lam2*b
                r3=zr(lx+6)*(1.d0-alp(i))+zr(lx+3)*alp(i)-z2+2.d0*&
                lam2*c
                q1=zr(lx+4)*(1.d0-alp(i))+zr(lx+1)*alp(i)-x1+2.d0*&
                lam1*a
                q2=zr(lx+5)*(1.d0-alp(i))+zr(lx+2)*alp(i)-y1+2.d0*&
                lam1*b
                q3=zr(lx+6)*(1.d0-alp(i))+zr(lx+3)*alp(i)-z1+2.d0*&
                lam1*c
            endif
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
            s=sqrt(q1**2+q2**2+q3**2)
            q1=q1/s
            q2=q2/s
            q3=q3/s
            s=sqrt(r1**2+r2**2+r3**2)
            r1=r1/s
            r2=r2/s
            r3=r3/s
            s=f1*(q1-r1)+f2*(q2-r2)+f3*(q3-r3)
            s=s*.10d0*xl/d
            do 2 j = 1, 3
                force(j) = force(j)+s*u(j)*an1(i)
                force(j+3) = force(j+3)+s*w(j)*an2(i)
                force(j+6) = force(j+6)+s*u(j)*an3(i)
                force(j+9) = force(+j+9)+s*w(j)*an4(i)
 2          continue
30      continue
 1  end do
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
!
    call jedema()
end subroutine
