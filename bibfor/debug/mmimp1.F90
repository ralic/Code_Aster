subroutine mmimp1(ifm, mesh, ds_contact)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/mminfi.h"
#include "asterfort/mminfl.h"
#include "asterfort/mminfm.h"
#include "asterfort/mmnorm.h"
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    integer, intent(in) :: ifm
    character(len=8), intent(in) :: mesh
    type(NL_DS_Contact), intent(in) :: ds_contact
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE - IMPRESSIONS)
!
! AFFICHAGE APPARIEMENT
!
! ----------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  ds_contact       : datastructure for contact management
!
    integer :: ztabf
    integer :: posmae, nummae, nummam, numnoe, jdecme
    character(len=8) :: nommae, nommam, nomnoe
    real(kind=8) :: ksipc1, ksipc2, ksipr1, ksipr2, wpc, r8bid, seuili
    integer :: xs
    real(kind=8) :: tau1(3), tau2(3), norm(3)
    character(len=24) :: tabfin
    integer :: jtabf
    integer :: iptm, i_zone, imae, inoe, iptc
    integer :: model_ndim, nb_cont_zone, nnoe, nptm, nbmae
    integer :: ilcnx1
    aster_logical :: lveri
    integer, pointer :: connex(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
    write (ifm,*) '<CONTACT> ... RESULTAT DE L''APPARIEMENT'
!
! - Get contact parameters
!
    model_ndim = cfdisi(ds_contact%sdcont_defi,'NDIM' )
    nb_cont_zone = cfdisi(ds_contact%sdcont_defi,'NZOCO')
!
! --- RECUPERATION DE QUELQUES DONNEES
!
    tabfin = ds_contact%sdcont_solv(1:14)//'.TABFIN'
    call jeveuo(tabfin, 'L', jtabf)
    ztabf = cfmmvd('ZTABF')
!
! --- ACCES MAILLAGE
!
    call jeveuo(mesh//'.CONNEX', 'L', vi=connex)
    call jeveuo(jexatr(mesh//'.CONNEX', 'LONCUM'), 'L', ilcnx1)
!
! --- BOUCLE SUR LES ZONES
!
    iptc = 1
    do i_zone = 1, nb_cont_zone
!
! ----- MODE VERIF: ON SAUTE LES POINTS
!
        lveri = mminfl(ds_contact%sdcont_defi,'VERIF' ,i_zone )
        if (lveri) then
            goto 25
        endif
!
! ----- INFORMATION SUR LA ZONE
!
        jdecme = mminfi(ds_contact%sdcont_defi,'JDECME',i_zone )
        nbmae = mminfi(ds_contact%sdcont_defi,'NBMAE' ,i_zone )
!
! ----- BOUCLE SUR LES MAILLES ESCLAVES
!
        do imae = 1, nbmae
            posmae = jdecme + imae
!
! ------- NOMBRE DE POINTS DE CONTACT
!
            call mminfm(posmae, ds_contact%sdcont_defi, 'NPTM', nptm)
!
! ------- REPERAGE MAILLE ESCLAVE
!
            nummae = nint(zr(jtabf+ztabf*(iptc-1)+1))
            call jenuno(jexnum(mesh//'.NOMMAI', nummae), nommae)
            nnoe = zi(ilcnx1+nummae) - zi(ilcnx1-1+nummae)
!
! ------- INFOS SUR MAILLE ESCLAVE
!
            write (ifm,100) nommae,i_zone,nnoe,nptm
            do inoe = 1, nnoe
                numnoe = connex(1+zi(ilcnx1-1+nummae)-2+inoe)
                call jenuno(jexnum(mesh//'.NOMNOE', numnoe), nomnoe)
                write (ifm,101) nomnoe
            end do
!
! ------- BOUCLE SUR LES POINTS
!
            do iptm = 1, nptm
!
! --------- POINT DE CONTACT EN COURS
!
                write(ifm,200) iptm
                ksipc1 = zr(jtabf+ztabf*(iptc-1)+3)
                ksipc2 = zr(jtabf+ztabf*(iptc-1)+4)
                wpc = zr(jtabf+ztabf*(iptc-1)+14)
                write(ifm,300) ksipc1,ksipc2,wpc

!
! --------- REPERAGE MAILLE MAITRE
!
                nummam = nint(zr(jtabf+ztabf*(iptc-1)+2))
                call jenuno(jexnum(mesh//'.NOMMAI', nummam), nommam)
!
! --------- ETAT DU NOEUD
!
                ksipr1 = zr(jtabf+ztabf*(iptc-1)+5)
                ksipr2 = zr(jtabf+ztabf*(iptc-1)+6)
                if (zr(jtabf+ztabf*(iptc-1)+18) .eq. 1.d0) then
                    write(ifm,*) '<CONTACT>        EXCLUS CONTACT    '
                else if (zr(jtabf+ztabf*(iptc-1)+19) .ge. 1.d0) then
                    write(ifm,*) '<CONTACT>        EXCLUS FROTTEMENT '
                else
                    write(ifm,203) nommam,ksipr1,ksipr2
                endif
!
! --------- REPERE LOCAL
!
                tau1(1) = zr(jtabf+ztabf*(iptc-1)+7 )
                tau1(2) = zr(jtabf+ztabf*(iptc-1)+8 )
                tau1(3) = zr(jtabf+ztabf*(iptc-1)+9 )
                tau2(1) = zr(jtabf+ztabf*(iptc-1)+10)
                tau2(2) = zr(jtabf+ztabf*(iptc-1)+11)
                tau2(3) = zr(jtabf+ztabf*(iptc-1)+12)
                write(ifm,202) tau1(1),tau1(2),tau1(3), tau2(1),tau2( 2),tau2(3)
                call mmnorm(model_ndim, tau1, tau2, norm, r8bid)
                write(ifm,201) norm(1),norm(2),norm(3)
!
! --------- ETAT DE CONTACT
!
                xs = nint(zr(jtabf+ztabf*(iptc-1)+22))
!
                if (xs .eq. 0) then
                    write(ifm,701)
                else if (xs.eq.1) then
                    write(ifm,700)
                else
                    ASSERT(.false.)
                endif
!
! --------- AUTRES INFOS
!
                seuili = zr(jtabf+ztabf*(iptc-1)+16)
                write(ifm,402) seuili

!
! --------- LIAISON SUIVANTE
!
                iptc = iptc + 1
!
            end do
        end do
 25     continue
    end do
    
!
101 format (' <CONTACT>        NOEUD :',a8)
100 format (' <CONTACT>     * MAILLE ESCLAVE ',a8,' ( ZONE ',i5,') - (',i5,' NOEUDS ) - (',&
             i5,' POINTS DE CONTACT )' )
200 format (' <CONTACT>     ** POINT DE CONTACT ',i3)
300 format (' <CONTACT>        SITUE EN  : <', e10.3,',',e10.3,'> - POIDS INTEGRATION: ',e10.3)
201 format (' <CONTACT>        NORMALE   : <', e10.3,',',e10.3,',',e10.3,'>')
202 format (' <CONTACT>        TANGENTES : <', e10.3,',',e10.3,',',e10.3,'> <',&
                                        e10.3,',',e10.3,',',e10.3,'>')
203 format (' <CONTACT>        SE PROJETTE SUR LA MAILLE MAITRE ', a8,' EN  <',e10.3,',',e10.3,'>')
402 format (' <CONTACT>        SEUIL_INIT : <',e10.3,'>')
700 format (' <CONTACT>        ETAT : EN CONTACT')
701 format (' <CONTACT>        ETAT : PAS EN CONTACT')
!
    call jedema()
!
end subroutine
