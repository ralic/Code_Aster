subroutine preflx(graexc, mailla, chamat, celem, npdsc3,&
                  iadsc3, nindex, ilnoex, lifex2)
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
!***********************************************************************
!    C. DUVAL
!-----------------------------------------------------------------------
!  BUT: MODIFIER L INTERSPECTRE EXCITATION POUR BIEN REPRESENTER LES
    implicit none
!      SOURCES FLUIDES EXCITATION
!        (CALCUL DYNAMIQUE ALEATOIRE)
!
!-----------------------------------------------------------------------
!
! GRAEXC   /IN /: GRANDEUR EXCITATION
! MAILLA   /IN /: CONCEPT MAILLAGE
! CHAMAT   /IN /: CONCEPT CHAMP_MATER
! CELEM    /IN /: CONCEPT CARA_ELEM
! NPDSC3   /IN /: NOMBRE DE FREQUENCES DANS LA DISCRETISATION
! IADSC3   /IN /: POINTEUR DANS ZR DU DEBUT DES FREQUENCES DISCRETISEES
! NINDEX   /IN /: NOMBRE D INDICE UTILES DANS L INTESPECTRE EXCITATION
! ILNOEX   /IN /: POINTEUR DANS ZK8 DES NOEUDS EXCITATION
! LIFEX2   /IN /: TABLEAU DES ADRESSES DES DEBUTS D INTERSP
!                 EXCITATION
! LIFEX2   /OUT/: IDEM
!
!
!
!
!
#include "jeveux.h"
!
#include "asterc/r8pi.h"
#include "asterfort/jecreo.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jeveut.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/reseci.h"
#include "asterfort/wkvect.h"
    character(len=8) :: nmnoe1, nmnoe2, chamat, celem, mailla, mater, kbid
    character(len=16) :: graexc
    character(len=24) :: k24bd1
    character(len=24) :: lifex2, lifex3
!
!-----------------------------------------------------------------------
    integer :: iad1, iaddx,  iadfx2, iadfx3, iadlma, iadr
    integer :: iadrho, iadsc3, iadsec, iapp1, iapp1b, iapp2, iapp2b
    integer :: icode, idec1, iexc1, ifreq1, igrma1, igrma2, ij1
    integer :: ij2, ilfex2, ilfex3, ilien1, ilien2, ilima, ilnoex
    integer :: ima1, imai1, imai2, imai3, inbfx3, inbmai, inbnoe
    integer :: ingrma, inlien, inoe1, inoe2, inuno1, inuno2, inuno3
    integer :: inuno4, inurho, invalk, ipar1, ipar2, ivalk1, nindex
    integer :: nmalim, npdsc3
    real(kind=8) :: dx, dx1, dx2, omega, pi, rho
    real(kind=8) :: rho1, rho2, sect1, sect2, sign, x1, x2
    real(kind=8) :: y1, y2, z1, z2
    integer, pointer :: desc(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
    if (graexc(1:5) .ne. 'SOUR_') goto 9999
!
    pi=r8pi()
!
!
!----1----RECUPERATION DE RHO DX SECTFLUIDE POUR LES SOURCE FLUIDES
!
    call wkvect('&&OP0131.RHO', 'V V R8', nindex, iadrho)
    call wkvect('&&OP0131.DX', 'V V R8', nindex, iaddx)
    call wkvect('&&OP0131.SECTFLUID', 'V V R8', 2*nindex, iadsec)
    do 306 iexc1 = 1, nindex
!
!-----ON RECHERCHE LA PREMIERE MAILLE CONTENANT LE NOEUD:IMAI1
!
        if ((graexc.eq.'SOUR_PRESS') .or. (graexc.eq.'SOUR_FORCE')) then
            inoe1=2*(iexc1-1)+1
        else
            inoe1=iexc1
        endif
        nmnoe1=zk8(ilnoex-1+inoe1)
        call jenonu(jexnom(mailla//'.NOMNOE', nmnoe1), inuno1)
!
!----RECUPERATION DE DX DISTANCE ENTRE LES DEUX POINTS DE LA SOURCE
!
        if ((graexc.eq.'SOUR_PRESS') .or. (graexc.eq. 'SOUR_FORCE')) then
!
            nmnoe2=zk8(ilnoex-1+inoe1+1)
            call jenonu(jexnom(mailla//'.NOMNOE', nmnoe2), inuno2)
            call jeveuo(mailla//'.COORDO    .VALE', 'L', iad1)
            x1=zr(iad1-1+(inuno1-1)*3+1)
            y1=zr(iad1-1+(inuno1-1)*3+2)
            z1=zr(iad1-1+(inuno1-1)*3+3)
            x2=zr(iad1-1+(inuno2-1)*3+1)
            y2=zr(iad1-1+(inuno2-1)*3+2)
            z2=zr(iad1-1+(inuno2-1)*3+3)
            dx=sqrt((x2-x1)**2+(y2-y1)**2+(z2-z1)**2)
            zr(iaddx-1+iexc1)=dx
!
        endif
!------------
        k24bd1=mailla//'.CONNEX'
        call jelira(k24bd1, 'NMAXOC', inbmai)
        inoe1=1
        inoe2=1
        do 307 imai1 = 1, inbmai
            call jeveuo(jexnum(k24bd1, imai1), 'L', iad1)
            call jelira(jexnum(k24bd1, imai1), 'LONMAX', inbnoe)
            do 308 inoe1 = 1, inbnoe
                inuno3=zi(iad1-1+inoe1)
                if (inuno1 .eq. inuno3) then
                    inoe2=100
                    if ((graexc.eq.'SOUR_PRESS') .or. (graexc.eq. 'SOUR_FORCE')) then
                        do 319 inoe2 = 1, inbnoe
                            inuno4=zi(iad1-1+inoe2)
                            if (inuno2 .eq. inuno4) goto 309
319                      continue
                    else
                        goto 309
                    endif
                endif
308          continue
307      continue
309      continue
!
!-------ON RECUPERE LES SECTIONS FLUIDES DE LA MAILLE IMAI1
!
        if (inoe1 .lt. inoe2) then
            call reseci(celem, imai1, zr(iadsec-1+2*(iexc1-1)+1), zr(iadsec-1+2*(iexc1-1)+2))
        else
            call reseci(celem, imai1, zr(iadsec-1+2*(iexc1-1)+2), zr(iadsec-1+2*(iexc1-1)+1))
        endif
!
!
!--------ON RECHERCHE LE MATERIAU CORRESPONDANT AU GROUPE DE MAILLES
!        IL EST REPERE PAR SON NUMERO D ORDRE ILIEN1
!
        call jeveuo(chamat//'.CHAMP_MAT .DESC', 'L', vi=desc)
        inlien=desc(3)
        do 310 ilien1 = 1, inlien
            icode=desc(1+3-1+2*(ilien1-1)+1)
            if (icode .eq. 1) then
!----- -----DANS CE CAS TOUTES LES MAILLES ONT LE MEME CHAMAT
                ilien2=1
                goto 311
            else if (icode.eq.2) then
!
!----- -----DANS CE CAS LA MAILLE A ETE DEFINIE LORS DU MAILLA
!--------ON RECHERCHE LE PREMIER GROUP_MAI QUI CONTIENT LA MAILLE
!        ET QUI A UN MATERIAU AFFECTE
!
                call jelira(mailla//'.GROUPEMA', 'NUTIOC', ingrma)
                do 312 igrma1 = 1, ingrma
                    call jeveuo(jexnum(mailla//'.GROUPEMA', igrma1), 'L', iad1)
                    call jelira(jexnum(mailla//'.GROUPEMA', igrma1), 'LONUTI', inbmai)
                    do 313 imai2 = 1, inbmai
                        imai3=zi(iad1-1+imai2)
                        if (imai3 .eq. imai1) then
                            goto 314
                        endif
313                  continue
312              continue
314              continue
                igrma2=desc(1+3-1+2*ilien1)
                if (igrma1 .eq. igrma2) then
                    ilien2=ilien1
                    goto 311
                endif
            else if (icode.eq.3) then
!----------DANS CE CAS LA MAILLE A ETE DEFINIE PAR AFFE_MATERIAU
                ilima=desc(1+3-1+2*ilien1)
                call jeveuo(jexnum(chamat//'.CHAMP_MAT .LIMA', ilima), 'L', iadlma)
                call jelira(jexnum(chamat//'.CHAMP_MAT .LIMA', ilima), 'LONMAX', nmalim)
                do 326 ima1 = 1, nmalim
                    if (zi(iadlma-1+ima1) .eq. imai1) then
                        ilien2=ilien1
                        goto 311
                    endif
326              continue
            endif
310      continue
311      continue
!
!--------POUR LE LIEN ILIEN2 ON VA RECUPERER LE MATERIAU PUIS
!        LA MASSE VOLUMIQUE
!
        call jeveuo(chamat//'.CHAMP_MAT .VALE', 'L', iad1)
        mater=zk8(iad1-1+ilien2)
        k24bd1=mater//'.FLUIDE    .VALK'
        call jeveuo(k24bd1, 'L', iad1)
        call jelira(k24bd1, 'LONMAX', invalk)
        do 317 ivalk1 = 1, invalk
            kbid=zk8(iad1-1+ivalk1)
            if (kbid(1:3) .eq. 'RHO') then
                inurho=ivalk1
                goto 318
            endif
317      continue
318      continue
        k24bd1=mater//'.FLUIDE    .VALR'
        call jeveuo(k24bd1, 'L', iad1)
        rho=zr(iad1-1+inurho)
        zr(iadrho-1+iexc1)=rho
306  end do
!
!---2-----MULTIPLICATION PAR LE BON COEF :
!          RHOI*RHOJ*OMEGA**2 POUR DEBIT VOLUME
!          OMEGA**2             POUR DEBIT MASSE
!          SECT1*SECT2/DX1/DX2  POUR SAUT DE PRESSION
!          1.         /DX1/DX2  POUR SAUT DE FORCE
!
!
    call jeveuo(lifex2, 'L', ilfex2)
    do 320 iapp1 = 1, nindex
        do 321 iapp2 = iapp1, nindex
            ij1=((iapp2-1)*iapp2)/2+iapp1
            iadr=zi(ilfex2-1+ij1)
            do 323 ifreq1 = 1, npdsc3
                idec1=npdsc3+2*(ifreq1-1)+1
                if (graexc .eq. 'SOUR_DEBI_VOLU') then
                    rho1=zr(iadrho-1+iapp1)
                    rho2=zr(iadrho-1+iapp2)
                    omega=zr(iadsc3-1+ifreq1)*2.d0*pi
                    zr(iadr-1+idec1)=zr(iadr-1+idec1)*rho1*rho2*(&
                    omega**2)
                    zr(iadr-1+idec1+1)=zr(iadr-1+idec1+1)*rho1*rho2*(&
                    omega**2)
                else if (graexc.eq.'SOUR_DEBI_MASS') then
                    omega=zr(iadsc3-1+ifreq1)*2.d0*pi
                    zr(iadr-1+idec1)=zr(iadr-1+idec1)*(omega**2)
                    zr(iadr-1+idec1+1)=zr(iadr-1+idec1+1)*(omega**2)
                else if (graexc.eq.'SOUR_PRESS') then
                    sect1=zr(iadsec-1+2*(iapp1-1)+1)
                    sect2=zr(iadsec-1+2*(iapp2-1)+1)
                    dx1=zr(iaddx-1+iapp1)
                    dx2=zr(iaddx-1+iapp2)
                    zr(iadr-1+idec1)=zr(iadr-1+idec1)*sect1*sect2/dx1/&
                    dx2
                    zr(iadr-1+idec1+1)=zr(iadr-1+idec1+1)*sect1*sect2/&
                    dx1/dx2
                else if (graexc.eq.'SOUR_FORCE') then
                    dx1=zr(iaddx-1+iapp1)
                    dx2=zr(iaddx-1+iapp2)
                    zr(iadr-1+idec1)=zr(iadr-1+idec1)/dx1/dx2
                    zr(iadr-1+idec1+1)=zr(iadr-1+idec1+1)/dx1/dx2
                endif
323          continue
321      continue
320  end do
!
!
!---3-----DUPLICATION DE L INTERSPECTRE DANS LE CAS DE SOURCE DE
!          PRESSION OU DE FORCE
!
    if ((graexc.eq.'SOUR_FORCE') .or. (graexc.eq.'SOUR_PRESS')) then
!
!
        inbfx3=(2*nindex*(2*nindex+1))/2
        lifex3 = '&&OP0131.LIADFX3'
        call wkvect('&&OP0131.LIADFX3', 'V V I', inbfx3, ilfex3)
        do 322 iapp1 = 1, 2*nindex
            ipar1=mod(iapp1,2)
            do 324 iapp2 = iapp1, 2*nindex
                ipar2=mod(iapp2,2)
                if (ipar1 .eq. ipar2) then
                    sign=1.d0
                else
                    sign=-1.d0
                endif
                write(k24bd1,'(A8,A3,2I4.4,A5)')'&&OP0131','.F3',&
                iapp1,iapp2, '.VALE'
                ij1=(iapp2*(iapp2-1))/2+iapp1
                call jecreo(k24bd1, 'V V R8')
                call jeecra(k24bd1, 'LONMAX', npdsc3*3)
                call jeecra(k24bd1, 'LONUTI', npdsc3*3)
                call jeveut(k24bd1, 'E', zi(ilfex3-1+ij1))
                iadfx3=zi(ilfex3-1+ij1)
                iapp1b=(iapp1+ipar1)/2
                iapp2b=(iapp2+ipar2)/2
                ij2=(iapp2b*(iapp2b-1))/2+iapp1b
                iadfx2=zi(ilfex2-1+ij2)
                do 325 ifreq1 = npdsc3+1, 3*npdsc3
                    zr(iadfx3-1+ifreq1)=zr(iadfx2-1+ifreq1)*sign
325              continue
324          end do
322      end do
        lifex2=lifex3
!
    endif
9999  continue
    call jedema()
end subroutine
