subroutine jeveuo(nomlu, cel, jadr, &
             & vl,vi,vi4,vr,vc,vk8,vk16,vk24,vk32,vk80)
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
! person_in_charge: j-pierre.lefebvre at edf.fr
! aslint: disable=W0405,C1002,W1304
      use iso_c_binding, only:  c_loc, c_ptr, c_f_pointer
      implicit none
#include "jeveux.h"
#include "jeveux_private.h"
#include "asterfort/jjallc.h"
#include "asterfort/jjalty.h"
#include "asterfort/jjcroc.h"
#include "asterfort/jjvern.h"
#include "asterfort/jxlocs.h"
#include "asterfort/utmess.h"
#include "asterfort/jelira.h"
#include "asterfort/jgetlmx.h"
#include "asterfort/assert.h"
#include "asterfort/jgetptc.h"

    character(len=*), intent(in) :: nomlu
    character(len=*), intent(in) :: cel
    integer, optional :: jadr

    logical,           pointer, optional, intent(out) :: vl(:)
    integer,           pointer, optional, intent(out) :: vi(:)
    integer(kind=4),   pointer, optional, intent(out) :: vi4(:)
    real(kind=8),      pointer, optional, intent(out) :: vr(:)
    complex(kind=8),   pointer, optional, intent(out) :: vc(:)
    character(len=8),  pointer, optional, intent(out) :: vk8(:)
    character(len=16), pointer, optional, intent(out) :: vk16(:)
    character(len=24), pointer, optional, intent(out) :: vk24(:)
    character(len=32), pointer, optional, intent(out) :: vk32(:)
    character(len=80), pointer, optional, intent(out) :: vk80(:)


!   ==================================================================
    integer :: lk1zon, jk1zon, liszon, jiszon
    common /izonje/  lk1zon , jk1zon , liszon , jiszon
!-----------------------------------------------------------------------
    integer :: ibacol, iblono, inat, inatb, ixdeso, ixiadd, ixlono
    integer :: jcara, jdate, jdocu, jgenr, jhcod, jiadd, jiadm
    integer :: jlong, jlono, jltyp, jluti, jmarq, jorig, jrnom
    integer :: jtype, lonoi, ltypi, n
!-----------------------------------------------------------------------
    parameter  ( n = 5 )
    common /jiatje/  jltyp(n), jlong(n), jdate(n), jiadd(n), jiadm(n),&
     &                 jlono(n), jhcod(n), jcara(n), jluti(n), jmarq(n)
!
    common /jkatje/  jgenr(n), jtype(n), jdocu(n), jorig(n), jrnom(n)
    integer :: numatr
    common /idatje/  numatr
!     ------------------------------------------------------------------
    integer :: iclas, iclaos, iclaco, idatos, idatco, idatoc
    common /iatcje/  iclas ,iclaos , iclaco , idatos , idatco , idatoc
    integer :: izr, izc, izl, izk8, izk16, izk24, izk32, izk80
    equivalence    (izr,zr),(izc,zc),(izl,zl),(izk8,zk8),(izk16,zk16),&
     &               (izk24,zk24),(izk32,zk32),(izk80,zk80)
! ----------------------------------------------------------------------
    character(len=1) :: genri, typei, kcel
    character(len=8) :: noml8
    character(len=32) :: noml32
    integer :: icre, iret
    integer :: iddeso, idiadd, idlono
    parameter    (  iddeso = 1 , idiadd = 2  , idlono = 8   )

    integer :: jad, n1, jctab
    character(len=8) :: ktyp
    type(c_ptr) :: pc

!   ==================================================================
    noml32 = nomlu
    noml8 = noml32(25:32)
    kcel = cel
    if (kcel .ne. 'L' .and. kcel .ne. 'E') then
        call utmess('F', 'JEVEUX1_27', sk=kcel)
    endif
!
    icre = 0
    call jjvern(noml32, icre, iret)
    inat = iret
    inatb = iret
    select case (iret)
! ----   IRET = 0
    case (0)
        call utmess('F', 'JEVEUX_26', sk=noml32(1:24))
!
! ----   IRET = 1
    case (1)
        genri = genr( jgenr(iclaos) + idatos )
        typei = type( jtype(iclaos) + idatos )
        ltypi = ltyp( jltyp(iclaos) + idatos )
        if (genri .eq. 'N') then
            call utmess('F', 'JEVEUX1_20', sk=noml32)
        endif
! ----   IRET = 2
    case (2)
        call jjallc(iclaco, idatco, cel, ibacol)
        ixiadd = iszon ( jiszon + ibacol + idiadd )
        ixdeso = iszon ( jiszon + ibacol + iddeso )
        if (noml8 .eq. '$$XATR  ') then
            ixlono = numatr
            iblono = iadm ( jiadm(iclaco) + 2*ixlono-1 )
            genri = genr ( jgenr(iclaco) + ixlono )
            ltypi = ltyp ( jltyp(iclaco) + ixlono )
            lonoi = lono ( jlono(iclaco) + ixlono ) * ltypi
            call jxlocs(zi, genri, ltypi, lonoi, iblono,&
                        .false., jctab)
            goto 100
        else
            if (noml8 .ne. ' ') then
                inat = 3
                call jjcroc(noml8, icre)
!            ------ CAS D'UN OBJET DE COLLECTION  ------
                if (ixiadd .ne. 0) inatb = 3
            else
                if (ixiadd .ne. 0) then
!            ----------- COLLECTION DISPERSEE
                    call utmess('F', 'JEVEUX1_21', sk=noml32)
                endif
            endif
            genri = genr( jgenr(iclaco) + ixdeso )
            typei = type( jtype(iclaco) + ixdeso )
            ltypi = ltyp( jltyp(iclaco) + ixdeso )
        endif
!
    end select
!
    call jjalty(typei, ltypi, cel, inatb, jctab)
    if (inat .eq. 3 .and. ixiadd .eq. 0) then
        ixlono = iszon ( jiszon + ibacol + idlono )
        if (ixlono .gt. 0) then
            iblono = iadm ( jiadm(iclaco) + 2*ixlono-1 )
            lonoi = iszon(jiszon+iblono-1+idatoc+1) - iszon(jiszon+ iblono-1+idatoc )
            if (lonoi .gt. 0) then
                jctab = jctab + (iszon(jiszon+iblono-1+idatoc) - 1)
            else
                call utmess('F', 'JEVEUX1_22', sk=noml32)
            endif
        else
            jctab = jctab + long(jlong(iclaco)+ixdeso) * (idatoc-1)
        endif
    endif
100   continue


!     -- cas : on demande l'adresse :
!     --------------------------------
      jad=jctab
      if (present(jadr)) then
          jadr=jad
          goto 999
      endif


!     -- cas : on demande un pointeur sur un vecteur :
!     ------------------------------------------------
      call jgetlmx(noml32,n1)
      call jelira(noml32,'TYPELONG',cval=ktyp)


      if (present(vl)) then
          ASSERT(ktyp.eq.'L')
          call jgetptc(jad,pc,vl=zl(1))
          call c_f_pointer(pc,vl,[n1])

      elseif (present(vi)) then
          ASSERT(ktyp.eq.'I')
          call jgetptc(jad,pc,vi=zi(1))
          call c_f_pointer(pc,vi,[n1])

      elseif (present(vi4)) then
          ASSERT(ktyp.eq.'S')
          call jgetptc(jad,pc,vi4=zi4(1))
          call c_f_pointer(pc,vi4,[n1])

      elseif (present(vr)) then
          ASSERT(ktyp.eq.'R')
          call jgetptc(jad,pc,vr=zr(1))
          call c_f_pointer(pc,vr,[n1])

      elseif (present(vc)) then
          ASSERT(ktyp.eq.'C')
          call jgetptc(jad,pc,vc=zc(1))
          call c_f_pointer(pc,vc,[n1])

      elseif (present(vk8)) then
          ASSERT(ktyp.eq.'K8')
          call jgetptc(jad,pc,vk8=zk8(1))
          call c_f_pointer(pc,vk8,[n1])

      elseif (present(vk16)) then
          ASSERT(ktyp.eq.'K16')
          call jgetptc(jad,pc,vk16=zk16(1))
          call c_f_pointer(pc,vk16,[n1])

      elseif (present(vk24)) then
          ASSERT(ktyp.eq.'K24')
          call jgetptc(jad,pc,vk24=zk24(1))
          call c_f_pointer(pc,vk24,[n1])

      elseif (present(vk32)) then
          ASSERT(ktyp.eq.'K32')
          call jgetptc(jad,pc,vk32=zk32(1))
          call c_f_pointer(pc,vk32,[n1])

      elseif (present(vk80)) then
          ASSERT(ktyp.eq.'K80')
          call jgetptc(jad,pc,vk80=zk80(1))
          call c_f_pointer(pc,vk80,[n1])

      else
          ASSERT(.false.)
      endif

999   continue

end subroutine
