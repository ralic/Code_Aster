subroutine canort(noma, nbma, listma, ndim, nbno,&
                  listno, type_calc)
!
    implicit none
!
#include "jeveux.h"
#include "asterc/r8rddg.h"
#include "asterfort/armin.h"
#include "asterfort/assert.h"
#include "asterfort/cncinv.h"
#include "asterfort/codree.h"
#include "asterfort/dffno.h"
#include "asterfort/jecreo.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/ltnotb.h"
#include "asterfort/norlin.h"
#include "asterfort/provec.h"
#include "asterfort/tbliva.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
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
!
    character(len=8), intent(in) :: noma
    integer, intent(in) :: nbma
    integer, intent(in) :: listma(*)
    integer, intent(in) :: ndim
    integer, intent(in) :: nbno
    integer, intent(in) :: listno(*)
    integer, intent(in) :: type_calc
!
! --------------------------------------------------------------------------------------------------
!
! AFFE_CHAR_MECA
!
! Compute normals and tangents at nodes
!
! --------------------------------------------------------------------------------------------------
!
!
! In  noma   : mesh
! In  nbma   : number of elements
! In  listma : list of elements
! In  ndim : space dimension
! In  nbno : number of nodes
! In  listno : list of nodes
! In  type_calc : normals (type_calc = 1) or tangents (type_calc = 2)
!
! Objects created:
!     &&CANORT.NORMALE : normals at nodes
!     &&CANORT.TANGENT : tangents at nodes
!
! --------------------------------------------------------------------------------------------------
!
    integer :: dimcoo, i, ifonc, ibid, iret, jnorm, isom, in
    integer :: idobj2,    ij, ino
    integer :: n, nocc, nno, nnos, nnn
    integer :: iinver, imail, numail, ityp, jdes, nn, numno, lino(9)
    real(kind=8) :: coor(3, 9), a, b, c, pvec(3), norme
    character(len=8) :: kangl, knumai
    character(len=8) :: nomtyp, nomnoe
    character(len=24) :: nomobj, nomob2, coninv
    character(len=24) :: valk(2)
    real(kind=8) :: dfse2(4), dfse3(9), prec
    real(kind=8) :: dftr3(18), dftr6(72), dftr7(98)
    real(kind=8) :: dfqu4(32), dfqu8(128), dfqu9(162)
    real(kind=8) :: eksix, eksiy, eksiz, eetax, eetay, eetaz
    real(kind=8) :: vnorm, cosvec, sinvec, angl, atan2
    real(kind=8), pointer :: vale(:) => null()
    integer, pointer :: desc(:) => null()
    integer, pointer :: typmail(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
!     RECUPERATION DES FONCTIONS DE FORMES POUR TOUS LES
!     TYPES D ELEMENTS SUSCEPTIBLES D ETRE PRESENT
!
    call dffno('SE2', ibid, nno, nnos, dfse2)
    call dffno('SE3', ibid, nno, nnos, dfse3)
    call dffno('TR3', ibid, nno, nnos, dftr3)
    call dffno('TR6', ibid, nno, nnos, dftr6)
    call dffno('TR7', ibid, nno, nnos, dftr7)
    call dffno('QU4', ibid, nno, nnos, dfqu4)
    call dffno('QU8', ibid, nno, nnos, dfqu8)
    call dffno('QU9', ibid, nno, nnos, dfqu9)
    coninv='&&CANORT.CONINV'
!
! - Creation of resultant object
!
    if (type_calc .eq. 1) nomobj = '&&CANORT.NORMALE'
    if (type_calc .eq. 2) nomobj = '&&CANORT.TANGENT'
    call jedetr(nomobj)
    call jecreo(nomobj, 'V V R')
    call jeecra(nomobj, 'LONMAX', ndim*nbno)
    call jeveuo(nomobj, 'E', jnorm)
!
    call jeveuo(noma//'.COORDO    .VALE', 'L', vr=vale)
    call jeveuo(noma//'.TYPMAIL', 'L', vi=typmail)
!
    prec = armin(noma)*1.d-06
    ASSERT(abs(nbma).gt.0)
!
!     RECUPERATION DE LA CONNECTIVITE INVERSE
    call cncinv(noma, listma, abs(nbma), 'V', coninv)
!
    nomob2 = '&&CANORT.VECTEUR'
    call jeexin(nomob2, iret)
    call jedetr(nomob2)
    isom = 0
    do i = 1, nbno
        call jelira(jexnum(coninv, listno(i)), 'LONMAX', nnn)
        isom = isom + nnn
    end do
!
    call wkvect(nomob2, 'V V R', ndim*isom, idobj2)
!
    call jeveuo(noma//'.COORDO    .DESC', 'L', vi=desc)
!
    ij=0
!     BOUCLE SUR TOUS LES NOEUDS CONCERNES
    do ino = 1, nbno
        numno=listno(ino)
        call jelira(jexnum(coninv, numno), 'LONMAX', nnn)
        call jeveuo(jexnum(coninv, numno), 'L', iinver)
!
!    BOUCLE SUR TOUTES LES MAILLES CONNECTEES AU NOEUD ACTUEL
        do imail = 1, nnn
!
!           NUMERO ABSOLUE DE LA MAILLE
!
            numail=listma(zi(iinver-1+imail))
            ityp=typmail(numail)
            call jenuno(jexnum('&CATA.TM.NOMTM', ityp), nomtyp)
            call jeveuo(jexnum(noma//'.CONNEX', numail), 'L', jdes)
            call jelira(jexnum(noma//'.CONNEX', numail), 'LONMAX', nn)
            if (ndim .eq. 2 .and. nomtyp(1:4) .eq. 'SEG2') then
                dimcoo = -desc(2)
                lino(1)=zi(jdes-1+1)
                lino(2)=zi(jdes-1+2)
                coor(1,1)=vale(dimcoo*(lino(1)-1)+1)
                coor(2,1)=vale(dimcoo*(lino(1)-1)+2)
                coor(1,2)=vale(dimcoo*(lino(2)-1)+1)
                coor(2,2)=vale(dimcoo*(lino(2)-1)+2)
                eksix=coor(1,1)*dfse2(1)+coor(1,2)*dfse2(2)
                eksiy=coor(2,1)*dfse2(1)+coor(2,2)*dfse2(2)
                if (type_calc .eq. 2) then
                    norme=sqrt(eksix**2+eksiy**2)
                    if (norme .gt. prec) then
                        a=eksix/norme
                        b=eksiy/norme
                    else
                        call jenuno(jexnum(noma//'.NOMMAI', numail), knumai)
                        call utmess('F', 'CHARGES2_23', sk=knumai)
                    endif
                else if (type_calc .eq.1) then
                    norme=sqrt(eksix**2+eksiy**2)
                    if (norme .gt. prec) then
                        a=eksiy/norme
                        b=-eksix/norme
                    else
                        call jenuno(jexnum(noma//'.NOMMAI', numail), knumai)
                        call utmess('F', 'CHARGES2_24', sk=knumai)
                    endif
                endif
                zr(jnorm-1+2*(ino-1)+1)=zr(jnorm-1+2*(ino-1)+1)&
                +a/nnn
                zr(jnorm-1+2*(ino-1)+2)=zr(jnorm-1+2*(ino-1)+2)&
                +b/nnn
                ij=ij+1
                zr(idobj2-1+2*(ij-1)+1) = a
                zr(idobj2-1+2*(ij-1)+2) = b
            else if (ndim.eq.2.and.nomtyp(1:4).eq.'SEG3') then
                do i = 1, nn
                    dimcoo = -desc(2)
                    lino(i)=zi(jdes-1+i)
                    coor(1,i)=vale(dimcoo*(lino(i)-1)+1)
                    coor(2,i)=vale(dimcoo*(lino(i)-1)+2)
                    coor(3,i)=0.d0
                    if (numno .eq. lino(i)) in=i
                end do
                eksix=0.d0
                eksiy=0.d0
!              CALCUL DU  VECTEUR TANGENT VIA LES FONCTIONS DE FORMES
                do ifonc = 1, nn
                    eksix=eksix+coor(1,ifonc)*dfse3((in-1)*nn+ifonc)
                    eksiy=eksiy+coor(2,ifonc)*dfse3((in-1)*nn+ifonc)
                end do
!              ON S INTERESSE AU VECTEUR TANGENT
                if (type_calc .eq. 2) then
                    norme=sqrt(eksix**2+eksiy**2)
                    if (norme .gt. prec) then
                        a=eksix/norme
                        b=eksiy/norme
                    else
                        call norlin('SE3', 2, knumai, coor, dfse2,&
                                    in, prec, a, b, c)
                    endif
!
!              ON S INTERESSE AU VECTEUR NORMAL
                else if (type_calc .eq.1) then
                    norme=sqrt(eksix**2+eksiy**2)
                    if (norme .gt. prec) then
                        a=eksiy/norme
                        b=-eksix/norme
                    else
                        call norlin('SE3', 1, knumai, coor, dfse2,&
                                    in, prec, a, b, c)
                    endif
                endif
                zr(jnorm-1+2*(ino-1)+1)=zr(jnorm-1+2*(ino-1)+1)&
                +a/nnn
                zr(jnorm-1+2*(ino-1)+2)=zr(jnorm-1+2*(ino-1)+2)&
                +b/nnn
                ij=ij+1
                zr(idobj2-1+2*(ij-1)+1) = a
                zr(idobj2-1+2*(ij-1)+2) = b
            else if (ndim.eq.3.and.nomtyp(1:3).eq.'SEG') then
                call utmess('F', 'CHARGES2_25')
!
            else if (ndim.eq.3.and.nomtyp(1:5).eq.'QUAD4') then
                do i = 1, nn
                    lino(i)=zi(jdes-1+i)
                    coor(1,i)=vale(3*(lino(i)-1)+1)
                    coor(2,i)=vale(3*(lino(i)-1)+2)
                    coor(3,i)=vale(3*(lino(i)-1)+3)
                    if (numno .eq. lino(i)) in=i
                end do
                eksix=0.d0
                eksiy=0.d0
                eksiz=0.d0
                eetax=0.d0
                eetay=0.d0
                eetaz=0.d0
!
!              CALCUL DES DEUX VECTEURS TANGENTS
                do ifonc = 1, nn
                    eksix=eksix+coor(1,ifonc)*dfqu4((in-1)*nn*2+ifonc)
                    eksiy=eksiy+coor(2,ifonc)*dfqu4((in-1)*nn*2+ifonc)
                    eksiz=eksiz+coor(3,ifonc)*dfqu4((in-1)*nn*2+ifonc)
!
                    eetax=eetax+coor(1,ifonc)*dfqu4((in-1)*nn*2+nn+&
                    ifonc)
                    eetay=eetay+coor(2,ifonc)*dfqu4((in-1)*nn*2+nn+&
                    ifonc)
                    eetaz=eetaz+coor(3,ifonc)*dfqu4((in-1)*nn*2+nn+&
                    ifonc)
                end do
!
!              CALCUL DU VECTEUR NORMAL ET NORMALISATION
                a=eksiy*eetaz-eksiz*eetay
                b=eksiz*eetax-eksix*eetaz
                c=eksix*eetay-eksiy*eetax
                norme=sqrt(a*a+b*b+c*c)
                if (norme .gt. prec) then
                    a=a/norme
                    b=b/norme
                    c=c/norme
                else
                    call jenuno(jexnum(noma//'.NOMMAI', numail), knumai)
                    call utmess('F', 'CHARGES2_26', sk=knumai)
                endif
!              ON FAIT LA MOYENNE SUR TOUTES LES MAILLES DES NORMALES
!              RELATIVES A UN NOEUD
                zr(jnorm-1+3*(ino-1)+1)=zr(jnorm-1+3*(ino-1)+1)&
                +a/nnn
                zr(jnorm-1+3*(ino-1)+2)=zr(jnorm-1+3*(ino-1)+2)&
                +b/nnn
                zr(jnorm-1+3*(ino-1)+3)=zr(jnorm-1+3*(ino-1)+3)&
                +c/nnn
                ij=ij+1
!              ON STOCHE DANS L OBJET IDOBJ2 TOUTES LES NORMALES POUR
!              UNE VERIFICATION ULTERIEURE
                zr(idobj2-1+3*(ij-1)+1) = a
                zr(idobj2-1+3*(ij-1)+2) = b
                zr(idobj2-1+3*(ij-1)+3) = c
            else if (ndim.eq.3.and.nomtyp(1:5).eq.'QUAD8') then
                do i = 1, nn
                    lino(i)=zi(jdes-1+i)
                    coor(1,i)=vale(3*(lino(i)-1)+1)
                    coor(2,i)=vale(3*(lino(i)-1)+2)
                    coor(3,i)=vale(3*(lino(i)-1)+3)
                    if (numno .eq. lino(i)) in=i
                end do
                eksix=0.d0
                eksiy=0.d0
                eksiz=0.d0
                eetax=0.d0
                eetay=0.d0
                eetaz=0.d0
!              CALCUL DES DEUX VECTEURS TANGENTS
                do ifonc = 1, nn
!
                    eksix=eksix+coor(1,ifonc)*dfqu8((in-1)*nn*2+ifonc)
                    eksiy=eksiy+coor(2,ifonc)*dfqu8((in-1)*nn*2+ifonc)
                    eksiz=eksiz+coor(3,ifonc)*dfqu8((in-1)*nn*2+ifonc)
!
                    eetax=eetax+coor(1,ifonc)*dfqu8((in-1)*nn*2+nn+&
                    ifonc)
                    eetay=eetay+coor(2,ifonc)*dfqu8((in-1)*nn*2+nn+&
                    ifonc)
                    eetaz=eetaz+coor(3,ifonc)*dfqu8((in-1)*nn*2+nn+&
                    ifonc)
                end do
!              CALCUL DU VECTEUR NORMAL ET NORMALISATION
                a=eksiy*eetaz-eksiz*eetay
                b=eksiz*eetax-eksix*eetaz
                c=eksix*eetay-eksiy*eetax
                norme=sqrt(a*a+b*b+c*c)
                if (norme .gt. prec) then
                    a=a/norme
                    b=b/norme
                    c=c/norme
                else
                    call norlin('QU8', 0, knumai, coor, dfqu4,&
                                in, prec, a, b, c)
                endif
!              ON FAIT LA MOYENNE SUR TOUTES LES MAILLES DES NORMALES
!              RELATIVES A UN NOEUD
                zr(jnorm-1+3*(ino-1)+1)=zr(jnorm-1+3*(ino-1)+1)&
                +a/nnn
                zr(jnorm-1+3*(ino-1)+2)=zr(jnorm-1+3*(ino-1)+2)&
                +b/nnn
                zr(jnorm-1+3*(ino-1)+3)=zr(jnorm-1+3*(ino-1)+3)&
                +c/nnn
                ij=ij+1
!              ON STOCHE DANS L OBJET IDOBJ2 TOUTES LES NORMALES POUR
!              UNE VERIFICATION ULTERIEURE
                zr(idobj2-1+3*(ij-1)+1) = a
                zr(idobj2-1+3*(ij-1)+2) = b
                zr(idobj2-1+3*(ij-1)+3) = c
            else if (ndim.eq.3.and.nomtyp(1:5).eq.'QUAD9') then
                do i = 1, nn
                    lino(i)=zi(jdes-1+i)
                    coor(1,i)=vale(3*(lino(i)-1)+1)
                    coor(2,i)=vale(3*(lino(i)-1)+2)
                    coor(3,i)=vale(3*(lino(i)-1)+3)
                    if (numno .eq. lino(i)) in=i
                end do
                eksix=0.d0
                eksiy=0.d0
                eksiz=0.d0
                eetax=0.d0
                eetay=0.d0
                eetaz=0.d0
!              CALCUL DES DEUX VECTEURS TANGENTS
                do ifonc = 1, nn
                    eksix=eksix+coor(1,ifonc)*dfqu9((in-1)*nn*2+ifonc)
                    eksiy=eksiy+coor(2,ifonc)*dfqu9((in-1)*nn*2+ifonc)
                    eksiz=eksiz+coor(3,ifonc)*dfqu9((in-1)*nn*2+ifonc)
!
                    eetax=eetax+coor(1,ifonc)*dfqu9((in-1)*nn*2+nn+&
                    ifonc)
                    eetay=eetay+coor(2,ifonc)*dfqu9((in-1)*nn*2+nn+&
                    ifonc)
                    eetaz=eetaz+coor(3,ifonc)*dfqu9((in-1)*nn*2+nn+&
                    ifonc)
                end do
!              CALCUL DU VECTEUR NORMAL ET NORMALISATION
                a=eksiy*eetaz-eksiz*eetay
                b=eksiz*eetax-eksix*eetaz
                c=eksix*eetay-eksiy*eetax
                norme=sqrt(a*a+b*b+c*c)
                if (norme .gt. prec) then
                    a=a/norme
                    b=b/norme
                    c=c/norme
                else
                    call jenuno(jexnum(noma//'.NOMMAI', numail), knumai)
                    call utmess('F', 'CHARGES2_26', sk=knumai)
                endif
!              ON FAIT LA MOYENNE SUR TOUTES LES MAILLES DES NORMALES
!              RELATIVES A UN NOEUD
                zr(jnorm-1+3*(ino-1)+1)=zr(jnorm-1+3*(ino-1)+1)&
                +a/nnn
                zr(jnorm-1+3*(ino-1)+2)=zr(jnorm-1+3*(ino-1)+2)&
                +b/nnn
                zr(jnorm-1+3*(ino-1)+3)=zr(jnorm-1+3*(ino-1)+3)&
                +c/nnn
                ij=ij+1
!              ON STOCHE DANS L OBJET IDOBJ2 TOUTES LES NORMALES POUR
!              UNE VERIFICATION ULTERIEURE
                zr(idobj2-1+3*(ij-1)+1) = a
                zr(idobj2-1+3*(ij-1)+2) = b
                zr(idobj2-1+3*(ij-1)+3) = c
            else if (ndim.eq.3.and.nomtyp(1:5).eq.'TRIA3') then
                do i = 1, nn
                    lino(i)=zi(jdes-1+i)
                    coor(1,i)=vale(3*(lino(i)-1)+1)
                    coor(2,i)=vale(3*(lino(i)-1)+2)
                    coor(3,i)=vale(3*(lino(i)-1)+3)
                    if (numno .eq. lino(i)) in=i
                end do
!              CALCUL DES DEUX VECTEURS TANGENTS
                eksix=0.d0
                eksiy=0.d0
                eksiz=0.d0
                eetax=0.d0
                eetay=0.d0
                eetaz=0.d0
                do ifonc = 1, nn
                    eksix=eksix+coor(1,ifonc)*dftr3((in-1)*nn*2+ifonc)
                    eksiy=eksiy+coor(2,ifonc)*dftr3((in-1)*nn*2+ifonc)
                    eksiz=eksiz+coor(3,ifonc)*dftr3((in-1)*nn*2+ifonc)
!
                    eetax=eetax+coor(1,ifonc)*dftr3((in-1)*nn*2+nn+&
                    ifonc)
                    eetay=eetay+coor(2,ifonc)*dftr3((in-1)*nn*2+nn+&
                    ifonc)
                    eetaz=eetaz+coor(3,ifonc)*dftr3((in-1)*nn*2+nn+&
                    ifonc)
                end do
!              CALCUL DU VECTEUR NORMAL ET NORMALISATION
                a=eksiy*eetaz-eksiz*eetay
                b=eksiz*eetax-eksix*eetaz
                c=eksix*eetay-eksiy*eetax
                norme=sqrt(a*a+b*b+c*c)
                if (norme .gt. prec) then
                    a=a/norme
                    b=b/norme
                    c=c/norme
                else
                    call jenuno(jexnum(noma//'.NOMMAI', numail), knumai)
                    call utmess('F', 'CHARGES2_26', sk=knumai)
                endif
!              ON FAIT LA MOYENNE SUR TOUTES LES MAILLES DES NORMALES
!              RELATIVES A UN NOEUD
                zr(jnorm-1+3*(ino-1)+1)=zr(jnorm-1+3*(ino-1)+1)&
                +a/nnn
                zr(jnorm-1+3*(ino-1)+2)=zr(jnorm-1+3*(ino-1)+2)&
                +b/nnn
                zr(jnorm-1+3*(ino-1)+3)=zr(jnorm-1+3*(ino-1)+3)&
                +c/nnn
                ij=ij+1
!              ON STOCHE DANS L OBJET IDOBJ2 TOUTES LES NORMALES POUR
!              UNE VERIFICATION ULTERIEURE
                zr(idobj2-1+3*(ij-1)+1) = a
                zr(idobj2-1+3*(ij-1)+2) = b
                zr(idobj2-1+3*(ij-1)+3) = c
            else if (ndim.eq.3.and.nomtyp(1:5).eq.'TRIA6') then
                do i = 1, nn
                    lino(i)=zi(jdes-1+i)
                    coor(1,i)=vale(3*(lino(i)-1)+1)
                    coor(2,i)=vale(3*(lino(i)-1)+2)
                    coor(3,i)=vale(3*(lino(i)-1)+3)
                    if (numno .eq. lino(i)) in=i
                end do
!              CALCUL DES DEUX VECTEURS TANGENTS
                eksix=0.d0
                eksiy=0.d0
                eksiz=0.d0
                eetax=0.d0
                eetay=0.d0
                eetaz=0.d0
                do ifonc = 1, nn
                    eksix=eksix+coor(1,ifonc)*dftr6((in-1)*nn*2+ifonc)
                    eksiy=eksiy+coor(2,ifonc)*dftr6((in-1)*nn*2+ifonc)
                    eksiz=eksiz+coor(3,ifonc)*dftr6((in-1)*nn*2+ifonc)
!
                    eetax=eetax+coor(1,ifonc)*dftr6((in-1)*nn*2+nn+&
                    ifonc)
                    eetay=eetay+coor(2,ifonc)*dftr6((in-1)*nn*2+nn+&
                    ifonc)
                    eetaz=eetaz+coor(3,ifonc)*dftr6((in-1)*nn*2+nn+&
                    ifonc)
                end do
!              CALCUL DU VECTEUR NORMAL ET NORMALISATION
                a=eksiy*eetaz-eksiz*eetay
                b=eksiz*eetax-eksix*eetaz
                c=eksix*eetay-eksiy*eetax
                norme=sqrt(a*a+b*b+c*c)
                if (norme .gt. prec) then
                    a=a/norme
                    b=b/norme
                    c=c/norme
                else
                    call norlin('TR6', 0, knumai, coor, dftr3,&
                                in, prec, a, b, c)
                endif
!              ON FAIT LA MOYENNE SUR TOUTES LES MAILLES DES NORMALES
!              RELATIVES A UN NOEUD
                zr(jnorm-1+3*(ino-1)+1)=zr(jnorm-1+3*(ino-1)+1)&
                +a/nnn
                zr(jnorm-1+3*(ino-1)+2)=zr(jnorm-1+3*(ino-1)+2)&
                +b/nnn
                zr(jnorm-1+3*(ino-1)+3)=zr(jnorm-1+3*(ino-1)+3)&
                +c/nnn
                ij=ij+1
!              ON STOCHE DANS L OBJET IDOBJ2 TOUTES LES NORMALES POUR
!              UNE VERIFICATION ULTERIEURE
                zr(idobj2-1+3*(ij-1)+1) = a
                zr(idobj2-1+3*(ij-1)+2) = b
                zr(idobj2-1+3*(ij-1)+3) = c
            else if (ndim.eq.3.and.nomtyp(1:5).eq.'TRIA7') then
                do i = 1, nn
                    lino(i)=zi(jdes-1+i)
                    coor(1,i)=vale(3*(lino(i)-1)+1)
                    coor(2,i)=vale(3*(lino(i)-1)+2)
                    coor(3,i)=vale(3*(lino(i)-1)+3)
                    if (numno .eq. lino(i)) in=i
                end do
!              CALCUL DES DEUX VECTEURS TANGENTS
                eksix=0.d0
                eksiy=0.d0
                eksiz=0.d0
                eetax=0.d0
                eetay=0.d0
                eetaz=0.d0
                do ifonc = 1, nn
                    eksix=eksix+coor(1,ifonc)*dftr7((in-1)*nn*2+ifonc)
                    eksiy=eksiy+coor(2,ifonc)*dftr7((in-1)*nn*2+ifonc)
                    eksiz=eksiz+coor(3,ifonc)*dftr7((in-1)*nn*2+ifonc)
!
                    eetax=eetax+coor(1,ifonc)*dftr7((in-1)*nn*2+nn+&
                    ifonc)
                    eetay=eetay+coor(2,ifonc)*dftr7((in-1)*nn*2+nn+&
                    ifonc)
                    eetaz=eetaz+coor(3,ifonc)*dftr7((in-1)*nn*2+nn+&
                    ifonc)
                end do
!              CALCUL DU VECTEUR NORMAL ET NORMALISATION
                a=eksiy*eetaz-eksiz*eetay
                b=eksiz*eetax-eksix*eetaz
                c=eksix*eetay-eksiy*eetax
                norme=sqrt(a*a+b*b+c*c)
                if (norme .gt. prec) then
                    a=a/norme
                    b=b/norme
                    c=c/norme
                else
                    call jenuno(jexnum(noma//'.NOMMAI', numail), knumai)
                    call utmess('F', 'CHARGES2_26', sk=knumai)
                endif
!              ON FAIT LA MOYENNE SUR TOUTES LES MAILLES DES NORMALES
!              RELATIVES A UN NOEUD
                zr(jnorm-1+3*(ino-1)+1)=zr(jnorm-1+3*(ino-1)+1)&
                +a/nnn
                zr(jnorm-1+3*(ino-1)+2)=zr(jnorm-1+3*(ino-1)+2)&
                +b/nnn
                zr(jnorm-1+3*(ino-1)+3)=zr(jnorm-1+3*(ino-1)+3)&
                +c/nnn
                ij=ij+1
!              ON STOCHE DANS L OBJET IDOBJ2 TOUTES LES NORMALES POUR
!              UNE VERIFICATION ULTERIEURE
                zr(idobj2-1+3*(ij-1)+1) = a
                zr(idobj2-1+3*(ij-1)+2) = b
                zr(idobj2-1+3*(ij-1)+3) = c
            else
                ASSERT(.false.)
            endif
        end do
    end do
!
!
    ij = 0
    do n = 1, nbno
        ino = listno(n)
        call jelira(jexnum(coninv, ino), 'LONMAX', nocc)
        if (ndim .eq. 2) then
            vnorm = zr(&
                    jnorm-1+2*(n-1)+1)*zr(jnorm-1+2*(n-1)+1) + zr(jnorm-1+2*(n-1)+2)*zr(jnorm-1+2&
                    &*(n-1)+2&
                    )
            vnorm = sqrt(vnorm)
            if (vnorm .lt. 1.0d-2) then
                call jenuno(jexnum(noma//'.NOMNOE', ino), nomnoe)
                call utmess('F', 'CHARGES2_30', sk=nomnoe)
            endif
            zr(jnorm-1+2*(n-1)+1)=zr(jnorm-1+2*(n-1)+1)/vnorm
            zr(jnorm-1+2*(n-1)+2)=zr(jnorm-1+2*(n-1)+2)/vnorm
            do i = 1, nocc
                ij = ij + 1
                cosvec = zr(&
                         jnorm-1+2*(n-1)+1)*zr(idobj2-1+2*(ij-1)+1) + zr(jnorm-1+2*(n-1)+2)*zr(id&
                         &obj2-1+2*(ij-1)+2&
                         )
                sinvec = zr(&
                         jnorm-1+2*(n-1)+1)*zr(idobj2-1+2*(ij-1)+2) - zr(jnorm-1+2*(n-1)+2)*zr(id&
                         &obj2-1+2*(ij-1)+1&
                         )
                angl = r8rddg()*atan2(sinvec,cosvec)
                if (abs(angl) .gt. 10.0d0) then
                    call jenuno(jexnum(noma//'.NOMNOE', ino), nomnoe)
                    call codree(abs(angl), 'G', kangl)
                    valk(1) = nomnoe
                    valk(2) = kangl
                    call utmess('A', 'CHARGES2_29', nk=2, valk=valk)
                endif
            enddo
        else if (ndim.eq.3) then
            vnorm = zr(&
                    jnorm-1+3*(n-1)+1)*zr(jnorm-1+3*(n-1)+1) + zr(jnorm-1+3*(n-1)+2)*zr(jnorm-1+3&
                    &*(n-1)+2) + zr(jnorm- 1+3*(n-1)+3)*zr(jnorm-1+3*(n-1)+3&
                    )
            vnorm = sqrt(vnorm)
            if (vnorm .lt. 1.0d-2) then
                call jenuno(jexnum(noma//'.NOMNOE', ino), nomnoe)
                call utmess('F', 'CHARGES2_30', sk=nomnoe)
            endif
            zr(jnorm-1+3*(n-1)+1)=zr(jnorm-1+3*(n-1)+1)/vnorm
            zr(jnorm-1+3*(n-1)+2)=zr(jnorm-1+3*(n-1)+2)/vnorm
            zr(jnorm-1+3*(n-1)+3)=zr(jnorm-1+3*(n-1)+3)/vnorm
            do i = 1, nocc
                ij = ij + 1
                cosvec = zr(&
                         jnorm-1+3*(n-1)+1)*zr(idobj2-1+3*(ij-1)+1) + zr(jnorm-1+3*(n-1)+2)*zr(id&
                         &obj2-1+3*(ij-1)+2) + zr(jnorm-1+3*(n-1)+3)*zr(idobj2-1+3*(ij-1)+3&
                         )
                call provec(zr(jnorm-1+3*(n-1)+1), zr(idobj2-1+3*(ij- 1)+1), pvec)
                sinvec = pvec(1)*pvec(1) + pvec(2)*pvec(2) + pvec(3)* pvec(3)
                sinvec = sqrt(sinvec)
                angl = r8rddg()*atan2(sinvec,cosvec)
                if (abs(angl) .gt. 10.0d0) then
                    call jenuno(jexnum(noma//'.NOMNOE', ino), nomnoe)
                    call codree(abs(angl), 'G', kangl)
                    valk(1) = nomnoe
                    valk(2) = kangl
                    call utmess('A', 'CHARGES2_29', nk=2, valk=valk)
                endif
            enddo
        endif
    end do
!
    call jedetr(nomob2)
    call jedetr(coninv)
    call jedema()
end subroutine
