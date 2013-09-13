subroutine op0038()
    implicit none
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ----------------------------------------------------------------------
!     COMMANDE:  CALC_CHAM_ELEM
!     - FONCTION REALISEE:
!         CALCUL DES FLUX ELEMENTAIRES EN THERMIQUE ;
!         CALCUL DE LA PRESSION ACOUSTIQUE ;
!         CALCUL DES COOREDONNEES DES POINTS DE GAUSS
!
! ----------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterc/r8vide.h"
#include "asterc/utalrm.h"
#include "asterfort/calcul.h"
#include "asterfort/cesvar.h"
#include "asterfort/chpver.h"
#include "asterfort/copisd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exlima.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mecham.h"
#include "asterfort/mechti.h"
#include "asterfort/medom1.h"
#include "asterfort/sdmpic.h"
#include "asterfort/utmess.h"
!
    integer :: ibid, ierd, iret, jcha, n1, n3, n4, n6, n7, nchar, nh
!
    real(kind=8) :: time, rundf
!
    character(len=1) :: base
    character(len=4) :: ctyp
    character(len=8) :: modele, cara, temp, noma, blan8, kmpic
    character(len=8) :: lpain(8), lpaout(1)
    character(len=16) :: type, oper, option
    character(len=19) :: kcha, chelem, press, ligrel
    character(len=24) :: chgeom, chcara(18), chharm, mate, k24b
    character(len=24) :: chtemp, chtime, chflug, chpres
    character(len=24) :: lchin(8), lchout(1)
!
    logical :: exitim
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infmaj()
!               123456789012345678901234
    k24b = '                        '
    blan8 = '        '
    kcha = '&&OP0038.CHARGES   '
!
    base = 'G'
    rundf = r8vide()
    press = ' '
    chtime = ' '
    exitim = .false.
!
    call getres(chelem, type, oper)
    call getvid(' ', 'ACCE', nbval=0, nbret=n1)
    if (n1 .ne. 0) then
        call utmess('A', 'CALCULEL3_96')
    endif
!
    call utalrm('OFF', 'CALCULEL3_40')
    call medom1(modele, mate, cara, kcha, nchar,&
                ctyp, blan8, 1)
    call utalrm('ON', 'CALCULEL3_40')
    call jeveuo(kcha//'.LCHA', 'E', jcha)
!
    call exlima(' ', 0, 'G', modele, ligrel)
!
    call getvtx(' ', 'OPTION', scal=option, nbret=n1)
    call getvid(' ', 'TEMP', scal=temp, nbret=n3)
    call getvid(' ', 'PRES', scal=press, nbret=n4)
    call getvr8(' ', 'INST', scal=time, nbret=n6)
    call getvis(' ', 'MODE_FOURIER', scal=nh, nbret=n7)
    if (n3 .ne. 0) then
        chtemp = temp
        call chpver('F', chtemp, 'NOEU', 'TEMP_R', ierd)
    endif
    if (n4 .ne. 0) then
        chpres = press
        call chpver('F', chpres, 'NOEU', 'PRES_C', ierd)
    endif
    if (n6 .ne. 0) exitim = .true.
    if (n7 .eq. 0) nh = 0
!
    call mecham(option, modele, cara, nh, chgeom,&
                chcara, chharm, iret)
    if (iret .ne. 0) goto 10
    noma = chgeom(1:8)
    if (exitim) call mechti(noma, time, rundf, rundf, chtime)
!
!        -------------------------
!        -- OPTIONS DE THERMIQUE :
!        -------------------------
    if (option(1:7) .eq. 'FLUX_EL') then
        chflug='&&OP0038.FLUXGAUSS'
        lchin(1)=chgeom
        lpain(1)='PGEOMER'
        lchin(2)=mate
        lpain(2)='PMATERC'
        lchin(3)=chcara(7)
        lpain(3)='PCACOQU'
        lchin(4)=chcara(12)
        lpain(4)='PCAMASS'
        lchin(5)=chtemp
        lpain(5)='PTEMPER'
        lchin(6)=chtime
        lpain(6)='PTEMPSR'
        lchin(7)=chharm
        lpain(7)='PHARMON'
        lchin(8)=k24b
        lpain(8)='PVARCPR'
        lchout(1)=chflug
        lpaout(1)='PFLUXPG'
!
        call calcul('S', 'FLUX_ELGA', ligrel, 8, lchin,&
                    lpain, 1, lchout, lpaout, 'V',&
                    'OUI')
!
        if (option .eq. 'FLUX_ELNO') then
            lchin(1)=chflug
            lpain(1)='PFLUXPG'
            lchout(1)=chelem
            lpaout(1)='PFLUXNO'
!
            call calcul('S', option, ligrel, 1, lchin,&
                        lpain, 1, lchout, lpaout, base,&
                        'OUI')
!
        else if (option.eq.'FLUX_ELGA') then
            call copisd('CHAMP', 'G', chflug, chelem)
        endif
!
!        ---------------------------
!        -- OPTION POINTS DE GAUSS :
!        ---------------------------
    else if (option.eq.'COOR_ELGA') then
        lchin(1)=chgeom
        lpain(1)='PGEOMER'
        lchin(2)=chcara(1)
        lpain(2)='PCAORIE'
        lchin(3)=chcara(17)
        lpain(3)='PFIBRES'
        lchin(4)=chcara(16)
        lpain(4)='PNBSP_I'
        lchin(5)=chcara(7)
        lpain(5)='PCACOQU'
        lchin(6)=chcara(5)
        lpain(6)='PCAGEPO'
        lchout(1)=chelem
        lpaout(1)='PCOORPG'
        call cesvar(cara, ' ', ligrel, lchout(1))
!
        call calcul('S', option, ligrel, 6, lchin,&
                    lpain, 1, lchout, lpaout, base,&
                    'OUI')
!        ------------------------
!        -- OPTIONS ACOUSTIQUES :
!        ------------------------
    else if (option.eq.'PRAC_ELNO') then
        lpain(1)='PPRESSC'
        lchin(1)=chpres
        lchout(1)=chelem
        lpaout(1)='PPRAC_R'
!
        call calcul('S', option, ligrel, 1, lchin,&
                    lpain, 1, lchout, lpaout, 'G',&
                    'OUI')
!
!        ----------------------
!        -- OPTIONS INCONNUES :
!        ----------------------
    else
        call utmess('F', 'CALCULEL3_22', sk=option)
    endif
!
10  continue
!
!     -- SI CHELEM N'EST PAS MPI_COMPLET, ON LE COMPLETE :
    call dismoi('F', 'MPI_COMPLET', chelem, 'CHAM_ELEM', ibid,&
                kmpic, ibid)
    if (kmpic .eq. 'NON') call sdmpic('CHAM_ELEM', chelem)
!
    call jedema()
end subroutine
