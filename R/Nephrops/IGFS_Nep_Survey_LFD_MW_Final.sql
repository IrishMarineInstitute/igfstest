		select a.metadata_id				
			,a.[Date]
			,year(a.[Date]) as Year
			,month(a.[Date]) as Month
			,a.Vessel
			,a.Functional_Unit
			,coalesce(a.Fishing_Grounds,'NoFishingGround') as Fishing_Grounds
			,a.SURVEY_CODE as Survey_Code
			,a.HAUL_STATION_NUMBER as Haul
			,b.Sample_Type
		   ,Case when b.[sample_category] like 'Fem%' then 'Female'
				  else b.[sample_category] end as Sex					
			,floor([LENGTH]) as CLmm
			,count(*) as NepCount
			,sum(c.A *Power([LENGTH],c.B)/1000) as PredWt_Kg
		From VMFSSSQL02.Nemesys.dbo.NEPHROPS_SAMPLE_METADATA a
		   join VMFSSSQL02.Nemesys.dbo.NEPHROPS_MEASURED_SAMPLE b
			  on a.metadata_id = b.metadata_id
		   join VMFSSSQL02.Nemesys.dbo.NEPHROPS_QC_FUNCTION_CONSTANTS c
			  on left(b.sample_category,1) = c.sex
			  and convert(varchar,a.Functional_Unit) = c.FU 
		--    join vmfssdev01.AnnexIIa.[dbo].[Stockman_Nephrops_Vessel_Logbook_Link_Trip_Basis_Typed_20140205_Vw] d
		--   on a.metadata_id = d.metadata_id
		where a.sample_type = 'SURVEY' 
		   and b.[Length] < 100 
		   and a.SURVEY_CODE like '%IGFS%'
		group by a.metadata_id				
			,a.[Date]
			,a.Vessel
			,a.Functional_Unit
		   ,coalesce(a.Fishing_Grounds,'NoFishingGround')
			,a.SURVEY_CODE
			,a.HAUL_STATION_NUMBER
			,b.Sample_Type
		   ,Case when b.[sample_category] like 'Fem%' then 'Female'
				  else b.[sample_category] end 			
			,floor([LENGTH])
		