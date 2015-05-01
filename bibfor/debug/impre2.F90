subroutine impre2(licoef, liddl, linoeu, libeta, indsur,&
                  ipntrl, nbterm, typcoe, typval, irela)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
#include "jeveux.h"
#include "asterfort/iunifi.h"
#include "asterfort/jeveuo.h"
    integer :: indsur, ipntrl, nbterm, irela
    character(len=24) :: licoef, liddl, linoeu, libeta
!
    integer :: idecal, idcoef, idnoeu, iddl, idbeta, ifm
    integer :: idcoe, idnoe, idl, i
    real(kind=8) :: dble, dimag
    character(len=4) :: typval, typcoe
!
    ifm = iunifi('MESSAGE')
!
! --- LISTE DES COEFFICIENTS :
!     ----------------------
    call jeveuo(licoef, 'L', idcoe)
!
! --- LISTE DES DDLS :
!     --------------
    call jeveuo(liddl, 'L', idl)
!
! --- LISTE DES NOMS DES NOEUDS :
!     -------------------------
    call jeveuo(linoeu, 'L', idnoe)
!
! --- NATURE ET VALEUR DU SECOND MEMBRE DE LA RELATION LINEAIRE :
!     ---------------------------------------------------------
    call jeveuo(libeta, 'L', idbeta)
!
    idecal = ipntrl - nbterm
    idcoef = idcoe + idecal
    idnoeu = idnoe + idecal
    iddl = idl + idecal
!
    if (indsur .eq. 1) then
        write(ifm,*) 'RELATION LINEAIRE REDONDANTE ET DONC SUPPRIMEE: '
!
! ---   IMPRESSION DE LA RELATION DANS LE CAS OU LES COEFFICIENTS
! ---   SONT REELS :
!       ----------
        if (typcoe .eq. 'REEL') then
            write(ifm,10)
            do 100 i = 1, nbterm-1
                write(ifm,20) zr(idcoef+i-1),zk8(iddl+i-1),zk8(idnoeu+&
                i-1)
100          continue
            write(ifm,90) zr(idcoef+nbterm-1),zk8(iddl+nbterm-1),&
            zk8(idnoeu+nbterm-1)
!
! ---   IMPRESSION DE LA RELATION DANS LE CAS OU LES COEFFICIENTS
! ---   SONT COMPLEXES :
!       --------------
        else if (typcoe.eq.'COMP') then
            write(ifm,30)
            do 200 i = 1, nbterm-1
                write(ifm,40) dble(zc(idcoef+i-1)), dimag(zc(idcoef+i-&
                1)), zk8(iddl+i-1),zk8(idnoeu+i-1)
200          continue
            write(ifm,95) dble(zc(idcoef+nbterm-1)), dimag(zc(idcoef+&
            nbterm-1)), zk8(iddl+nbterm-1),zk8(idnoeu+nbterm-1)
        endif
!
        if (typval .eq. 'REEL') then
            write(ifm,50) zr(idbeta+irela-1)
        else if (typval.eq.'FONC') then
            write(ifm,60) zk24(idbeta+irela-1)(1:19)
        else if (typval.eq.'COMP') then
            write(ifm,70) dble(zc(idbeta+irela-1)), dimag(zc(idbeta+&
            irela-1))
        endif
    endif
!
    write(ifm,80)
    10 format(2x,'    COEF     ','*','   DDL  ','(',' NOEUD  ',')')
    30 format(13x,'    COEF     ','*','   DDL  ','(',' NOEUD  ',')')
    20 format(2x,1pe12.5,' * ',2x,a6,'(',a8,')','+')
    90 format(2x,1pe12.5,' * ',2x,a6,'(',a8,')')
    40 format(2x,'(',1pe12.5,',',1pe12.5,')',' * ',&
     &       2x,a6,'(',a8,')','+')
    95 format(2x,'(',1pe12.5,',',1pe12.5,')',' * ',&
     &       2x,a6,'(',a8,')')
    50 format(2x,'=',1pe12.5)
    60 format(2x,'=',a19)
    70 format(2x,'=','(',1pe12.5,',',1pe12.5,')')
    80 format(2x,'______________________________________',//)
end subroutine
