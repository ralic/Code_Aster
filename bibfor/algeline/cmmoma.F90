subroutine cmmoma(mailla, momanu, nbno, nbnoaj)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/normev.h'
    include 'asterfort/provec.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/vdiff.h'
    include 'blas/ddot.h'
    integer :: nbno, nbnoaj
    character(len=*) :: mailla, momanu
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
!     OPERATEUR CREA_MAILLAGE   MOT CLE FACTEUR "MODI_MAILLE"
!     ------------------------------------------------------------------
!
    integer :: jmail, im, numa, jtyp, ityp, ino, jpt
    integer :: ittr6, ittr7, itqu8, itqu9, jvale, nuno, iatyma
    integer :: itse3, itse4, k
    real(kind=8) :: valr(3), w
    character(len=8) :: ma, nomail, nono1, nono2, nono3
    character(len=24) :: typmai, connex, cooval, canoma, canono
    character(len=24) :: valk(4)
!
    real(kind=8) :: coo1(3), coo2(3), coo3(3), theta, epsi, t13(3), t32(3)
    real(kind=8) :: normen, norme1, norme2, n(3), om(3), oc(3), c2, c6, t2, t6
    real(kind=8) :: t12(3)
    real(kind=8) :: n3m(3), mc(3), mp(3), mr(3), x3(3), x4(3), costet, dn1n2
    integer :: icoude, i, nuno1, nuno2, nuno3, ifm, niv
!     ------------------------------------------------------------------
!
    call jemarq()
!
    ma = mailla
    typmai = ma//'.TYPMAIL        '
    connex = ma//'.CONNEX         '
    cooval = ma//'.COORDO    .VALE'
    canoma = ma//'.NOMMAI'
    canono = ma//'.NOMNOE'
!
    call jeveuo(momanu, 'L', jmail)
    call jeveuo(cooval, 'E', jvale)
!
    call jenonu(jexnom('&CATA.TM.NOMTM', 'TRIA6'), ittr6)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'TRIA7'), ittr7)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'QUAD8'), itqu8)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'QUAD9'), itqu9)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'SEG3'), itse3)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'SEG4'), itse4)
!
    do 10 im = 1, nbnoaj
!
        numa = zi(jmail+im-1)
        call jeveuo(typmai, 'E', iatyma)
        jtyp=iatyma-1+numa
        ityp = zi(jtyp)
!
        if (ityp .eq. ittr6) then
            zi(jtyp) = ittr7
            ino = 7
!
        else if (ityp .eq. itqu8) then
            zi(jtyp) = itqu9
            ino = 9
!
        else if (ityp .eq. itse3) then
            zi(jtyp) = itse4
            ino = 4
!
        endif
!
        call jeveuo(jexnum(connex, numa), 'E', jpt)
!
        if (ityp .ne. itse3) then
            nuno = nbno + im
            zi(jpt+ino-1) = nuno
!
            if (ityp .eq. ittr6) then
!             -- TRIA6_7
                do 777, k=1,3
                w= 0.d0
                w= w + zr(jvale+3*(zi(jpt-1+1)-1)-1+k) * (-1.d0/&
                    9.d0)
                w= w + zr(jvale+3*(zi(jpt-1+2)-1)-1+k) * (-1.d0/&
                    9.d0)
                w= w + zr(jvale+3*(zi(jpt-1+3)-1)-1+k) * (-1.d0/&
                    9.d0)
!
                w= w + zr(jvale+3*(zi(jpt-1+4)-1)-1+k) * (4.d0/&
                    9.d0)
                w= w + zr(jvale+3*(zi(jpt-1+5)-1)-1+k) * (4.d0/&
                    9.d0)
                w= w + zr(jvale+3*(zi(jpt-1+6)-1)-1+k) * (4.d0/&
                    9.d0)
!
                zr(jvale+3*(nuno-1)-1+k) = w
777              continue
!
            else if (ityp.eq.itqu8) then
!             -- QUAD8_9
                do 778, k=1,3
                w= 0.d0
                w= w + zr(jvale+3*(zi(jpt-1+1)-1)-1+k) * (-1.d0/&
                    4.d0)
                w= w + zr(jvale+3*(zi(jpt-1+2)-1)-1+k) * (-1.d0/&
                    4.d0)
                w= w + zr(jvale+3*(zi(jpt-1+3)-1)-1+k) * (-1.d0/&
                    4.d0)
                w= w + zr(jvale+3*(zi(jpt-1+4)-1)-1+k) * (-1.d0/&
                    4.d0)
!
                w= w + zr(jvale+3*(zi(jpt-1+5)-1)-1+k) * (1.d0/&
                    2.d0)
                w= w + zr(jvale+3*(zi(jpt-1+6)-1)-1+k) * (1.d0/&
                    2.d0)
                w= w + zr(jvale+3*(zi(jpt-1+7)-1)-1+k) * (1.d0/&
                    2.d0)
                w= w + zr(jvale+3*(zi(jpt-1+8)-1)-1+k) * (1.d0/&
                    2.d0)
!
                zr(jvale+3*(nuno-1)-1+k) = w
778              continue
!
            else
                call assert(.false.)
            endif
!
        else
!
            do 30 i = 1, 3
                nuno1 = zi(jpt+1-1)
                coo1(i) = zr(jvale-1+3*(nuno1-1)+i )
                nuno2 = zi(jpt+2-1)
                coo2(i) = zr(jvale-1+3*(nuno2-1)+i )
                nuno3 = zi(jpt+3-1)
                coo3(i) = zr(jvale-1+3*(nuno3-1)+i )
30          continue
!
            call vdiff(3, coo3, coo1, t13)
            call vdiff(3, coo2, coo3, t32)
            call vdiff(3, coo2, coo1, t12)
            call normev(t13, norme1)
            call normev(t32, norme2)
            call normev(t12, dn1n2)
            call provec(t32, t13, n)
            call normev(n, normen)
            epsi=1.d-4*norme1
!
!           VERIF QUE LE 3EME NOEUD EST BIEN AU MILIEU
!
            if (abs(norme2-norme1) .gt. epsi) then
                call jenuno(jexnum(canoma, numa), nomail)
                call jenuno(jexnum(canono, nuno1), nono1)
                call jenuno(jexnum(canono, nuno2), nono2)
                call jenuno(jexnum(canono, nuno3), nono3)
                call infniv(ifm, niv)
                valr(1) = norme1
                valr(2) = norme2
                valr(3) = epsi
                valk(1) = nono3
                valk(2) = nono1
                valk(3) = nono2
                valk(4) = nomail
                call u2mesg('F', 'ALGELINE_23', 4, valk, 0,&
                            0, 3, valr)
            endif
!
            if (normen .le. epsi) then
                icoude=0
                theta = 0.d0
            else
                icoude=1
                costet=ddot(3,t13,1,t32,1)
                theta=2.d0*atan2(normen,costet)
            endif
!
            if (icoude .eq. 0) then
!
!           CAS DU TUYAU DROIT
!              SEGMENT N1-N2 DIVISE EN 3
                do 40 i = 1, 3
                    x3(i)=coo1(i)+t12(i)*dn1n2/3.d0
                    x4(i)=coo1(i)+2.d0*t12(i)*dn1n2/3.d0
40              continue
!
            else
!
!           CAS DU COUDE
!
                c2=cos(theta/2.d0)
                c6=cos(theta/6.d0)
                t2=tan(theta/2.d0)
                t6=tan(theta/6.d0)
!
                do 50 i = 1, 3
                    om(i)=(coo1(i)+coo2(i))*0.5d0
                    n3m(i)=om(i)-coo3(i)
                    mc(i)=n3m(i)*c2/(1.d0-c2)
                    oc(i)=om(i)+mc(i)
                    mp(i)=(coo1(i)-om(i))*t6/t2
                    mr(i)=(coo2(i)-om(i))*t6/t2
                    x3(i)=oc(i)+(mp(i)-mc(i))*c6/c2
                    x4(i)=oc(i)+(mr(i)-mc(i))*c6/c2
50              continue
            endif
!
!
            nuno = zi(jpt-1+3)
            zr(jvale+3*(nuno-1) ) = x3(1)
            zr(jvale+3*(nuno-1)+1) = x3(2)
            zr(jvale+3*(nuno-1)+2) = x3(3)
!
            nuno = nbno + im
            zr(jvale+3*(nuno-1) ) = x4(1)
            zr(jvale+3*(nuno-1)+1) = x4(2)
            zr(jvale+3*(nuno-1)+2) = x4(3)
!
            zi(jpt+ino-1) = nuno
!
        endif
!
10  end do
!
    call jedema()
end subroutine
