subroutine cesgno(ces1, celfpg, base, ces2)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
    implicit none
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterfort/assert.h"
#include "asterfort/cesexi.h"
#include "asterfort/dismoi.h"
#include "asterfort/elraca.h"
#include "asterfort/exisd.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jni002.h"
#include "asterfort/nuelrf.h"
!
    character(len=*) :: base
    character(len=24) :: celfpg
    character(len=19) :: ces2, ces1
! ------------------------------------------------------------------
! BUT: TRANSFORMER UN CHAM_ELEM_S/ELGA EN CHAM_ELEM_S/ELNO
! ------------------------------------------------------------------
!     ARGUMENTS:
! CES1  IN/JXIN  K19 : CHAM_ELEM_S A TRANSFORMER
!
! CELFPG IN/JXVAR  K24 :
!    NOM DE L'OBJET DECRIVANT LES FAMILLES DE P.G. DE CES1 (OU ' ')
!    CET OBJET N'EST UTILISE QUE SI ELGA -> ELNO
!    CET OBJET EST OBTENU PAR LA ROUTINE CELFPG.F
!  ATTENTION : CET OBJET EST EFFACE PENDANT L'OPERATION.
!
!
! BASE    IN      K1  : BASE DE CREATION POUR CES2 : G/V/L
! CES2    IN/JXVAR K19 : CHAM_ELEM_S RESULTAT
!         REMARQUE : LE CHAM_ELEM_S EST DEJA ALLOUE.
!-----------------------------------------------------------------------
!
!     ------------------------------------------------------------------
    integer :: nbnomx, nbfamx, nbpgmx
    parameter (nbnomx=27,nbfamx=20,nbpgmx=27)
    character(len=8) :: elrf, fapg1, fapg(nbfamx)
    real(kind=8) :: vol, x(3*nbnomx)
    integer :: nbfpg, nbpg(nbfamx), ndiml, nnol, nnosl
!
    integer :: ima, ncmp, icmp, ino, isp, nno
    integer :: nbma, iret
    integer :: npg, ipg, nujni, nbobj
    integer :: jces1k, jces1d, jces1l, jces1v, jces1c, iad1, nbpt1, nbsp1
    integer :: jces2d, jces2l, jces2v, iad2, nbpt2, nbsp2
    integer :: jmat, jganol, ivfl, jdfd2l, jcoopl, ipoidl, npgl, lonfam
    integer :: ifam, decal, jvr, idfdel, nufpg, avance, jnofpg
    character(len=8) :: ma, nomgd
    character(len=3) :: tsca
    character(len=16) :: schema
    character(len=24) :: liobj(10)
    real(kind=8) :: vrpg(nbpgmx), vrno(nbnomx), sr
    complex(kind=8) :: vcpg(nbpgmx), vcno(nbnomx), sc
!     ------------------------------------------------------------------
    call jemarq()
!
!
!
!     1. RECUPERATION DE :
!        MA     : NOM DU MAILLAGE
!        NOMGD  : NOM DE LA GRANDEUR
!        NCMP   : NOMBRE DE CMPS DE CES1
!        TSCA   : TYPE SCALAIRE DE LA GRANDEUR : R/C/I ...
!        NBMA   : NOMBRE DE MAILLES DU MAILLAGE
!        JCES1XX : ADRESSES CES1
!        JCES2XX : ADRESSES CES2
!        JNOFPG : ADRESSE DE CELFPG
!     --------------------------------------------------------------
    call exisd('CHAM_ELEM_S', ces1, iret)
    ASSERT(iret.gt.0)
    call jeveuo(ces1//'.CESK', 'L', jces1k)
    call jeveuo(ces1//'.CESC', 'L', jces1c)
    call jeveuo(ces1//'.CESD', 'L', jces1d)
    call jeveuo(ces1//'.CESV', 'L', jces1v)
    call jeveuo(ces1//'.CESL', 'L', jces1l)
    ma = zk8(jces1k-1+1)
    nomgd = zk8(jces1k-1+2)
    call dismoi('NB_MA_MAILLA', ma, 'MAILLAGE', repi=nbma)
    call jelira(ces1//'.CESC', 'LONMAX', ncmp)
    call dismoi('TYPE_SCA', nomgd, 'GRANDEUR', repk=tsca)
    ASSERT(tsca.eq.'R'.or.tsca.eq.'C')
!
    call jeveuo(ces2//'.CESD', 'L', jces2d)
    call jeveuo(ces2//'.CESV', 'E', jces2v)
    call jeveuo(ces2//'.CESL', 'E', jces2l)
!
    ASSERT(celfpg.ne.' ')
    call jeveuo(celfpg, 'E', jnofpg)
!
!
!
!     3. REMPLISSAGE DES OBJETS .CESL ET .CESV :
!     ------------------------------------------
!
!     POUR DES RAISONS DE PERFORMANCE, ON TRAITE TOUTES LES MAILLES
!     CORRESPONDANT AU MEME SCHEMA DE POINTS DE GAUSS
!
!     BOUCLE TANT QUE CELFPG N'EST PAS TOTALEMENT EFFACE (AVANCE=1):
!     ---------------------------------------------------------------
 10 continue
    schema = ' '
    avance = 0
!
    do ima = 1, nbma
        if (zk16(jnofpg-1+ima) .eq. ' ') goto 110
        if (schema .eq. ' ') schema = zk16(jnofpg-1+ima)
        if (zk16(jnofpg-1+ima) .ne. schema) goto 110
!
        avance = avance + 1
        schema = zk16(jnofpg-1+ima)
        elrf = schema(1:8)
        fapg1 = schema(9:16)
        zk16(jnofpg-1+ima) = ' '
!
!
!           3.1 : CALCUL DE LA MATRICE DE PASSAGE GA->NO
!                 (ON NE LE FAIT QUE POUR LA 1ERE MAILLE DU SCHEMA)
!           OUT : NPG,NNO,JMAT
!           --------------------------------------------------------
        if (avance .eq. 1) then
            call elraca(elrf, ndiml, nnol, nnosl, nbfpg,&
                        fapg, nbpg, x, vol)
            nufpg = indik8(fapg,fapg1,1,nbfpg)
            ASSERT(nufpg.gt.0)
            call nuelrf(elrf, nujni)
            ASSERT(nujni.eq.2)
            call jni002(elrf, 10, liobj, nbobj)
            call jeveuo('&INEL.'//elrf//'.ELRA_R', 'L', jvr)
!
            decal = 0
            do ifam = 1, nufpg - 1
                npgl = nbpg(ifam)
!
                lonfam = npgl
                lonfam = lonfam + npgl*ndiml
                lonfam = lonfam + npgl*nnol
                lonfam = lonfam + npgl*nnol*ndiml
                lonfam = lonfam + npgl*nnol*ndiml*ndiml
                lonfam = lonfam + 2 + npgl*nnol
!
                decal = decal + lonfam
            end do
!
            npgl = nbpg(nufpg)
!
            ipoidl = jvr + decal
            jcoopl = ipoidl + npgl
            ivfl = jcoopl + npgl*ndiml
            idfdel = ivfl + npgl*nnol
            jdfd2l = idfdel + npgl*nnol*ndiml
            jganol = jdfd2l + npgl*nnol*ndiml*ndiml
            nno = nint(zr(jganol-1+1))
            npg = nint(zr(jganol-1+2))
            jmat = jganol + 2
            ASSERT(nno.le.nbnomx)
            ASSERT(npg.le.nbpgmx)
        endif
!
!
!           3.2 : MULTIPLICATION PAR LA MATRICE
!           ------------------------------------
        nbpt1 = zi(jces1d-1+5+4* (ima-1)+1)
        nbsp1 = zi(jces1d-1+5+4* (ima-1)+2)
        nbpt2 = zi(jces2d-1+5+4* (ima-1)+1)
        nbsp2 = zi(jces2d-1+5+4* (ima-1)+2)
        ASSERT(nbsp1.eq.nbsp2)
        ASSERT(nbpt1.eq.npg)
        ASSERT(nbpt2.eq.nno)
!
!
        do icmp = 1, ncmp
            call cesexi('C', jces1d, jces1l, ima, 1,&
                        1, icmp, iad1)
            if (iad1 .le. 0) goto 100
!
            do isp = 1, nbsp1
!
!               -- RECOPIE DANS VXPG :
                do ipg = 1, npg
                    call cesexi('C', jces1d, jces1l, ima, ipg,&
                                isp, icmp, iad1)
                    ASSERT(iad1.gt.0)
                    if (tsca .eq. 'R') then
                        vrpg(ipg) = zr(jces1v-1+iad1)
!
                    else
                        vcpg(ipg) = zc(jces1v-1+iad1)
                    endif
                end do
!
!               -- MULTIPLICATION :
                if (tsca .eq. 'R') then
                    do ino = 1, nno
                        sr = 0.d0
                        do ipg = 1, npg
                            sr = sr + zr(jmat-1+ (ipg-1)*npg+ino)* vrpg(ipg)
                        end do
                        vrno(ino) = sr
                    end do
!
                else
                    do ino = 1, nno
                        sc = dcmplx(0.d0,0.d0)
                        do ipg = 1, npg
                            sc = sc + zr(jmat-1+ (ipg-1)*npg+ino)* vcpg(ipg)
                        end do
                        vcno(ino) = sc
                    end do
                endif
!
!
!               -- RECOPIE DE VXNO :
                do ino = 1, nno
                    call cesexi('C', jces2d, jces2l, ima, ino,&
                                isp, icmp, iad2)
                    ASSERT(iad2.lt.0)
                    if (tsca .eq. 'R') then
                        zr(jces2v-1-iad2) = vrno(ino)
                    else
                        zc(jces2v-1-iad2) = vcno(ino)
                    endif
                    zl(jces2l-1-iad2) = .true.
                end do
            end do
100         continue
        end do
!
110     continue
    end do
    if (avance .gt. 0) goto 10
!
!
!
    call jedema()
end subroutine
