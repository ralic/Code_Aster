subroutine alfint(chmatz   , imate, mate_namz, tdef, para_namz,&
                  mate_nume, prec , func_name)
!
implicit none
!
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/gettco.h"
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisdg.h"
#include "asterfort/gcncon.h"
#include "asterfort/jelira.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jeveut.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/rcvale.h"
#include "asterfort/utmess.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=*), intent(in) :: chmatz
    integer, intent(in) :: imate
    character(len=*), intent(in) :: mate_namz
    real(kind=8), intent(in) :: tdef
    character(len=*), intent(in) :: para_namz
    integer, intent(in) :: mate_nume
    real(kind=8), intent(in) :: prec
    character(len=19), intent(inout) :: func_name
!
! --------------------------------------------------------------------------------------------------
!
! Material - Coding
!
! Interpolation of ALPHA
!
! --------------------------------------------------------------------------------------------------
!
! In  chmate           : name of material field (CHAM_MATER)
! In  imate            : index of current material
! In  mate_name        : name of current material (MATER)
! In  tdef             : value of TEMP_DEF
! In  para_name        : name of parameter (ALPHA)
! In  mate_nume        : index of current material
! In  prec             : precision for interpolate (in DEFI_MATERIAU)
! IO  func_name        : function for ALPHA
!
! --------------------------------------------------------------------------------------------------
!
    integer :: icodre(1)
    character(len=8) :: k8dummy, chmate, mate_name, phys_name, valk(2)
    character(len=24) :: ktref
    character(len=32) :: phenom
    character(len=16) :: typres, nomcmd, para_name
    character(len=19) :: chwork
    integer :: nb_cmp, i, nbpts, jv_nomrc
    integer :: nbec, k, ec1, kk, ngdmax
    real(kind=8) :: undemi, tref, alfref(1), alphai, ti, tim1, tip1
    real(kind=8) :: alfim1, alfip1, dalref
    character(len=24), pointer :: v_prol(:) => null()
    real(kind=8), pointer :: v_func_vale(:) => null()
    real(kind=8), pointer :: v_work_vale(:) => null()
    integer, pointer :: v_chmate_desc(:) => null()
    character(len=8), pointer :: v_chmate_vale(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    chmate    = chmatz
    mate_name = mate_namz
    para_name = para_namz
    undemi    = 0.5d0
!
! - Not for thermic
!
    call getres(k8dummy, k8dummy, nomcmd)
    if (nomcmd(1:5) .eq. 'THER_') then
        goto 100
    endif
!
! - Access to CHAM_MATER
!
    call jeveuo(chmate//'.CHAMP_MAT .DESC', 'L', vi =v_chmate_desc)
    call jeveuo(chmate//'.CHAMP_MAT .VALE', 'L', vk8=v_chmate_vale)
!
! - Check physical quantity
!
    call jenuno(jexnum('&CATA.GD.NOMGD', v_chmate_desc(1)), phys_name)
    ASSERT(phys_name.eq.'NOMMATER')
    call jelira(jexnom('&CATA.GD.NOMCMP', 'NOMMATER'), 'LONMAX', nb_cmp)
    call dismoi('NB_EC', phys_name, 'GRANDEUR', repi=nbec)
    ngdmax=v_chmate_desc(2)
!
! - Get TREF (SUR LE 1ER ENTIER CODE)
!
    ec1 = v_chmate_desc(3+2*ngdmax+nbec*(imate-1)+1)
    k   = 0
    do kk = 1, 30
        if (exisdg([ec1],kk)) then
            k=k+1
        endif
    end do
    if (v_chmate_vale(nb_cmp*(imate-1)+k-3) .ne. 'TREF=>') then
        call utmess('F', 'MATERIAL1_56', sk=chmate)
    endif
    ktref(1:8)   = v_chmate_vale(nb_cmp*(imate-1)+k-2)
    ktref(9:16)  = v_chmate_vale(nb_cmp*(imate-1)+k-1)
    ktref(17:24) = v_chmate_vale(nb_cmp*(imate-1)+k)
    if (ktref(1:3) .eq. 'NAN') then
        goto 999
    endif
    read (ktref,'(1PE22.15)') tref
!
! - Get phenomen for material
!
    call jeveut(mate_name//'.MATERIAU.NOMRC', 'L', jv_nomrc)
    phenom = zk32(jv_nomrc+mate_nume-1)(1:10)
!
! --------------------------------------------------------------------------------------------------
!
!  Create new function for ALPHA
!
! --------------------------------------------------------------------------------------------------
!
!
! - Name of new function: new_name = &&old_name(1:2)//00NUMMAT
!
    call gcncon('.', chwork)
    chwork = '&&'//chwork(3:8)
!
! - Check if function
!
    call gettco(func_name, typres)
    if (typres .ne. 'FONCTION_SDASTER' .and. typres .ne. ' ') then
        call utmess('F', 'MATERIAL1_1')
    endif
!
! - Copy function
!
    call copisd('FONCTION', 'V', func_name, chwork)
!
! - Get number of points
!
    call jelira(chwork(1:19)//'.VALE', 'LONMAX', nbpts)
    nbpts = nbpts/2
!
! - Check function if only one point
!
    if (nbpts .eq. 1) then
        call jeveuo(chwork(1:19)//'.PROL', 'L', vk24=v_prol)
!        -- SI LA FONCTION EST UNE CONSTANTE, ON NE FAIT RIEN :
        if (v_prol(1) .eq. 'CONSTANT') then
            goto 100
!        -- SI TREF ET TDEF SONT PROCHES (1 DEGRE), ON NE FAIT RIEN :
        else if (abs(tref-tdef) .lt. 1.d0) then
            goto 100
        else
            call utmess('F', 'MATERIAL1_42', sk=func_name(1:8))
        endif
    endif
!
! - Get ALPHA at reference temperature tref
!
    call rcvale(mate_name, phenom, 1, 'TEMP    ', [tref],&
                1, para_name, alfref(1), icodre(1), 2)
!
! - Acces to function values
!
    call jeveuo(chwork(1:19)//'.VALE', 'E', vr=v_work_vale)
    call jeveuo(func_name(1:19)//'.VALE', 'L', vr=v_func_vale)
!
! - Compute new values for ALPHA
!
    do i = 1, nbpts
!
        alphai = v_func_vale(1+i+nbpts-1)
        ti = v_func_vale(i)
!
! --- DANS LE CAS OU ABS(TI-TREF) > PREC :
! --- ALPHA_NEW(TI) = (ALPHA(TI)*(TI-TDEF) - ALPHA(TREF)*(TREF-TDEF))
! ---                 /(TI-TREF)   :
        if (abs(ti-tref) .ge. prec) then
!
            v_work_vale(1+i+nbpts-1) = ( alphai*(ti-tdef)- alfref(1)*(tref- tdef)) /(ti-tref )
! --- DANS LE CAS OU ABS(TI-TREF) < PREC :
! --- IL FAUT D'ABORD CALCULER LA DERIVEE DE ALPHA PAR RAPPORT
! --- A LA TEMPERATURE EN TREF : D(ALPHA)/DT( TREF) :
        else
! ---   DANS LE CAS OU I > 1 ET I < NBPTS :
! ---   D(ALPHA)/DT( TREF) = 0.5*((ALPHA(TI+1)-ALPHA(TREF))/(TI+1-TREF)
! ---                            +(ALPHA(TREF)-ALPHA(TI-1))/(TREF-TI-1))
            if (i .gt. 1 .and. i .lt. nbpts) then
!
                tim1 = v_func_vale(1+i-1-1)
                tip1 = v_func_vale(1+i+1-1)
                alfim1 = v_func_vale(1+i+nbpts-1-1)
                alfip1 = v_func_vale(1+i+nbpts+1-1)
                if (tip1 .eq. tref) then
                    call utmess('F', 'MATERIAL1_3')
                endif
                if (tim1 .eq. tref) then
                    call utmess('F', 'MATERIAL1_3')
                endif
!
                dalref = undemi*((alfip1-alfref(1))/(tip1-tref) +(alfref(1)- alfim1)/(tref-tim1))
!
! ---   DANS LE CAS OU I = NBPTS :
! ---   D(ALPHA)/DT( TREF) = (ALPHA(TREF)-ALPHA(TI-1))/(TREF-TI-1) :
            else if (i.eq.nbpts) then
!
                tim1 = v_func_vale(1+i-1-1)
                alfim1 = v_func_vale(1+i+nbpts-1-1)
                if (tim1 .eq. tref) then
                    call utmess('F', 'MATERIAL1_3')
                endif
!
                dalref = (alfref(1)-alfim1)/(tref-tim1)
!
! ---   DANS LE CAS OU I = 1 :
! ---   D(ALPHA)/DT( TREF) = (ALPHA(TI+1)-ALPHA(TREF))/(TI+1-TREF) :
            else if (i.eq.1) then
!
                tip1 = v_func_vale(1+i+1-1)
                alfip1 = v_func_vale(1+i+nbpts+1-1)
                if (tip1 .eq. tref) then
                    call utmess('F', 'MATERIAL1_3')
                endif
!
                dalref = (alfip1-alfref(1))/(tip1-tref)
!
            endif
! ---   DANS CE CAS OU ABS(TI-TREF) < PREC , ON A :
! ---   ALPHA_NEW(TI) = ALPHA_NEW(TREF)
! ---   ET ALPHA_NEW(TREF) = D(ALPHA)/DT (TREF)*(TREF-TDEF)+ALPHA(TREF):
            v_work_vale(1+i+nbpts-1) = dalref*(tref-tdef) + alfref(1)
        endif
    end do
!
! - New function to save
!
    func_name = chwork
!
    goto 100
!     -- SECTION "ERREUR":
999 continue
    valk(1)=chmate
    valk(2)=mate_name
    call utmess('F', 'MATERIAL1_2', nk=2, valk=valk)
!
100 continue
end subroutine
