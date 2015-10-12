subroutine veri_seg(mailla, dmax_cable, lnuma, liproj, lidoubno,&
                    nbmaok, x3dca, iproj, n1, n2, numail)
    implicit none
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!  DESCRIPTION : 
!  -----------   
!       DEFI_CABLE_BP/RELA_CINE/CABLE-COQUE
!       ANALYSE LA LISTE DES SEGMENTS CANDIDATS A UNE PROJECTION DU 
!       NOEUD DE CABLE
!
!       POUR QUE LA PROJECTION SOIT POSSIBLE IL FAUT QU'IL EXISTE UNE
!       AUTRE MAILLE CONNECTEE AU SEGMENT CANDIDAT
!
!       IPROJ : 0 SI PROJECTION AUTORISE SUR UN SEGMENT
!              -1 SINON
!       OUT : N1, PREMIER NOEUD DU SEGMENT
!       OUT : N2, DEUXIEME NOEUD DU SEGMENT
!       OUT : NUMAIL, NUMERO DE LA MAILLE PORTANT LE SEGMENT
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
#include "asterfort/canorm.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/projtq.h"
#include "blas/daxpy.h"
#include "blas/dcopy.h"
!
! ARGUMENTS
! ---------
    character(len=8) :: mailla
    real(kind=8) :: x3dca(3), dmax_cable
    integer :: iproj, n1, n2, lnuma(*),lidoubno(*), liproj(*), nbmaok
    integer :: numail
!
! VARIABLES LOCALES
! -----------------
    integer :: imail, nb_seg, nn1, nn2, jconx1, jcoor, jconx2, nbcnx, inoma, noe
    integer :: jtyma, ntyma, itria, inoeu, icote, iproj2, i, jnoeu, jmail
    integer :: numail2, noe2, jseg
    integer, pointer :: liseg(:) => null()
    character(len=24) :: conxma, coorno, tymama
    real(kind=8) :: xyzma(3, 9), normal(3), excent, xbar(3), x3dp(3), prec
    real(kind=8) :: quart, d
    parameter (prec=1.d-2, quart = 0.25d0)
!
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
!

    AS_ALLOCATE(vi=liseg, size=nbmaok)
    nb_seg=0
    iproj = -1
    
    conxma = mailla//'.CONNEX'
    call jeveuo(conxma, 'L', jconx1)
    coorno = mailla//'.COORDO    .VALE'
    call jeveuo(coorno, 'L', jcoor)
    tymama = mailla//'.TYPMAIL'
    call jeveuo(tymama, 'L', jtyma)
    call jeveuo(jexatr(mailla//'.CONNEX', 'LONCUM'), 'L', jconx2)
!
    do imail = 1, nbmaok
        if (liproj(imail).eq.20)then
            n1 = lidoubno(3*imail-2)
            n2 = lidoubno(3*imail-1)
            numail = lnuma(imail)
!           on regarde d'abord s'il y a le iproj 20 complémentaire
            do jmail = imail+1, nbmaok
                if (liproj(jmail).eq.20)then
                    nn1 = lidoubno(3*jmail-2)
                    nn2 = lidoubno(3*jmail-1)
                    if (nn1 .eq. n1)then
                        if (nn2 .eq. n2)then
                            iproj = 0
                            goto 999
                        endif
                    elseif (nn1 .eq. n2)then
                        if (nn2 .eq. n1)then
                            iproj = 0
                            goto 999
                        endif
                    endif
                endif
            enddo
!           on regarde ensuite s'il existe une maille connectée à ce coté dans
!           la liste (cas où les précisions n'aurait pas misent cette maille en iproj 20)
            do jmail = 1, nbmaok
                if (liproj(jmail).ne.20)then
                    numail2 = lnuma(jmail)
                    nbcnx = zi(jconx2+numail2)-zi(jconx2-1+numail2)
                    do inoma = 1, nbcnx
                        noe = zi(jconx1-1+zi(jconx2+numail2-1)+inoma-1)
                        if (noe .eq. n1)then
                            if (inoma .eq.nbcnx) then
                                noe2 = zi(jconx1-1+zi(jconx2+numail2-1)+1-1)
                            else
                                noe2 = zi(jconx1-1+zi(jconx2+numail2-1)+inoma-1+1)
                            endif
                            if (noe .eq. n2)then
                                iproj = 0
                                goto 999
                            endif
                        elseif (noe .eq. n2)then
                            if (inoma .eq.nbcnx) then
                                noe2 = zi(jconx1-1+zi(jconx2+numail2-1)+1-1)
                            else
                                noe2 = zi(jconx1-1+zi(jconx2+numail2-1)+inoma-1+1)
                            endif
                            if (noe .eq. n1)then
                                iproj = 0
                                goto 999
                            endif
                        endif
                    enddo
                endif
            enddo
!           le segment est un bord du maillage, on garde les infos en mémoire
            nb_seg = nb_seg + 1 
            liseg(nb_seg) = numail
        endif
    enddo
!
!   tstbar est très sévère sur les cas limites
!   en cas d'echec on regarde si on est suffisamment près du bord pour
!   accepter la projection sur ce coté.
    do jseg = 1, nb_seg
        numail = liseg(jseg)
        nbcnx = zi(jconx2+numail)-zi(jconx2-1+numail)
        
        do inoma = 1, nbcnx
            noe = zi(jconx1-1+zi(jconx2+numail-1)+inoma-1)
            xyzma(1,inoma) = zr(jcoor+3*(noe-1) )
            xyzma(2,inoma) = zr(jcoor+3*(noe-1)+1)
            xyzma(3,inoma) = zr(jcoor+3*(noe-1)+2)
        enddo
!
        ntyma = zi(jtyma+numail-1)
        call canorm(xyzma, normal, 3, ntyma, 1)
!
        excent = normal(1)*(x3dca(1)-xyzma(1,1)) + normal(2)*( x3dca(2)-xyzma(2,1)) &
                         + normal(3)*(x3dca(3)-xyzma(3,1))
        call dcopy(3, x3dca, 1, x3dp, 1)  
        call daxpy(3, -excent, normal, 1, x3dp, 1)
!
        call projtq(nbcnx, xyzma, 1, x3dp, abs(excent), &
                    itria, inoeu, icote, xbar, iproj2)  
        ASSERT(iproj2 .eq.20)
        do i =1,3
            if(xbar(i).lt.0.d0 .and. abs(xbar(i)).le. quart)then
                inoeu = icote
                jnoeu = icote + 2
                if (jnoeu .eq. nbcnx + 1) jnoeu = 1
                if (jnoeu .eq. nbcnx + 2) jnoeu = 2
                d = sqrt((xyzma(1,jnoeu)-xyzma(1,inoeu))**2&
                    +(xyzma(2,jnoeu)-xyzma(2,inoeu))**2&
                    +(xyzma(3,jnoeu)-xyzma(3,inoeu))**2)
                if (d*abs(xbar(i)) .le. prec*dmax_cable)then
                    iproj = 0
                    goto 999
                endif
            endif
        enddo
    enddo    
!
999 continue
    AS_DEALLOCATE(vi=liseg)
!
end subroutine
